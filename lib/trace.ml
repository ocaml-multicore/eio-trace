module Read = Fxt.Read

module Rings = Map.Make(Int)
module Ids = Map.Make(Int)

let i64 = Int64.to_int

type timestamp = int64

type activation = [
  | `Pause
  | `Fiber of int
  | `Enter_span of string
  | `Exit_span
  | `Suspend_fiber of string
]

type event =
  | Log of string
  | Error of string
  | Create_cc of string * item
  | Add_fiber of { parent : int; child : item }
and item = {
  id : int;
  mutable name : string option;
  mutable parent : item option;
  mutable events : (timestamp * event) list;
  mutable inner_cc : int;
  mutable end_time : timestamp option;
  mutable activations : (timestamp * activation) list;
}

module Ring = struct
  type id = int

  type root = {
    mutable parent : (timestamp * int) option;
    mutable cc : (timestamp * item) option;
  }

  type event =
    | Suspend of string
    | Gc of string

  type t = {
    mutable current_fiber : int option;
    mutable events : (timestamp * event list) list;
    mutable roots : root list;
  }

  let push t ts e =
    let stack =
      match t.events with
      | [] -> []
      | (_, s) :: _ -> s
    in
    t.events <- (ts, e :: stack) :: t.events

  let pop t ts =
    let tail =
      match t.events with
      | (_, (_ :: x)) :: _ -> x
      | [] -> []
      | _ :: _ -> print_endline "warning: unmatched pop!"; []
    in
    t.events <- (ts, tail) :: t.events
end

type t = {
  mutable start_time : timestamp;
  mutable rings : Ring.t Rings.t;
  mutable items : item Ids.t;
}

let get_ring t ring =
  match Rings.find_opt ring t.rings with
  | Some x -> x
  | None ->
    let x = { Ring.current_fiber = None; events = []; roots = [] } in
    t.rings <- Rings.add ring x t.rings;
    x

let current_fiber t ring =
  let d = get_ring t ring in
  Option.map (fun id -> Ids.find id t.items) d.current_fiber

let get t id =
  match Ids.find_opt id t.items with
  | Some x -> x
  | None ->
    let x = { id; name = None; events = []; activations = []; inner_cc = id; parent = None; end_time = None } in
    t.items <- Ids.add id x t.items;
    x

let pp_item f x = Fmt.int f x.id

let add_activation item ts x =
  item.activations <- (ts, x) :: item.activations

let set_fiber t ring ts id =
  let d = get_ring t ring in
  current_fiber t ring |> Option.iter (fun old -> add_activation old ts `Pause);
  d.current_fiber <- Some id;
  let f = get t id in
  add_activation f ts (`Fiber id)

let id_of_pointer = function
  | `Pointer x -> i64 x
  | _ -> failwith "Not a pointer type!"

let ring_of_thread t (thread : Read.thread) =
  let id = i64 thread.tid in
  if id land 3 = 1 then (
    let ring = id lsr 2 in
    Some (get_ring t ring)
  ) else None

let fiber_of_thread t (thread : Read.thread) =
  let id = i64 thread.tid in
  if id land 3 = 2 then (
    let eio_id = id lsr 2 in
    Ids.find_opt eio_id t.items
  ) else None

let as_string = function
  | `String s -> s
  | _ -> failwith "Not a string"

let as_int64 = function
  | `Int64 x -> x
  | _ -> failwith "Not an int64 type!"

let process_event t e =
  let { Read.Event.ty; timestamp; thread; category; name; args } = e in
  t.start_time <- min t.start_time timestamp;
  match category, name, ty with
  | "eio", "cc", Duration_begin ->
    let id = List.assoc_opt "id" args |> Option.get |> id_of_pointer in
    let ty = List.assoc_opt "type" args |> Option.get |> as_string in
    let ring_id = List.assoc_opt "cpu" args |> Option.get |> as_int64 |> Int64.to_int in
    let ring = get_ring t ring_id in
    let cc = get t id in
    begin match fiber_of_thread t thread with
    | Some parent_fiber ->
        let parent_item = get t (parent_fiber.inner_cc) in
        parent_item.events <- (timestamp, Create_cc (ty, cc)) :: parent_item.events;
        cc.parent <- Some parent_item;
        parent_fiber.inner_cc <- id
    | None ->
        begin match ring.roots with
        | root :: _ when root.cc = None -> root.cc <- Some (timestamp, cc)
        | _ ->
          let root = { Ring.parent = None; cc = Some (timestamp, cc) } in
          ring.roots <- root :: ring.roots
        end
    end
  | "eio", "cc", Duration_end ->
    fiber_of_thread t thread |> Option.iter @@ fun fiber ->
    let inner_cc = fiber.inner_cc in
    let cc = get t inner_cc in
    cc.end_time <- Some timestamp;
    cc.parent |> Option.iter (fun parent_item ->
        fiber.inner_cc <- parent_item.id;
      )
  | "eio.span", name, Duration_begin ->
    begin match fiber_of_thread t thread with
      | Some fiber -> add_activation fiber timestamp (`Enter_span name)
      | None -> ring_of_thread t thread |> Option.iter (fun ring -> Ring.push ring timestamp (Suspend name))
    end
  | "eio.span", _name, Duration_end ->
    begin match fiber_of_thread t thread with
      | Some fiber -> add_activation fiber timestamp `Exit_span
      | None -> ring_of_thread t thread |> Option.iter (fun ring -> Ring.pop ring timestamp)
    end
  | "eio", "create-fiber", Instant ->
    let id = List.assoc_opt "id" args |> Option.get |> id_of_pointer in
    let cc = List.assoc_opt "cc" args |> Option.get |> id_of_pointer in
    let child = get t id in
    Ids.find_opt cc t.items |> Option.iter (fun cc ->
        let parent = fiber_of_thread t thread in
        let parent =
          match parent with
          | Some p -> p.id
          | None -> cc.id
        in
        cc.events <- (timestamp, Add_fiber { parent; child }) :: cc.events
      );
  | "eio", "log", Instant ->
    let msg = List.assoc_opt "message" args |> Option.get |> as_string in
    fiber_of_thread t thread |> Option.iter @@ fun fiber ->
    let item = get t (fiber.inner_cc) in
    item.events <- (timestamp, Log msg) :: item.events
  | "eio", "error", Instant ->
    let id = List.assoc_opt "id" args |> Option.get |> id_of_pointer in
    let msg = List.assoc_opt "message" args |> Option.get |> as_string in
    let item = get t id in
    item.events <- (timestamp, Error msg) :: item.events
  | "eio", "exit-fiber", Instant ->
    let id = List.assoc_opt "id" args |> Option.get |> id_of_pointer in
    let item = get t id in
    item.end_time <- Some timestamp
  | "eio.suspend", name, Duration_begin ->
    fiber_of_thread t thread |> Option.iter @@ fun fiber ->
    add_activation fiber timestamp (`Suspend_fiber name);
  | "eio", ("suspend-domain" as phase), Duration_begin
  | "gc", phase, Duration_begin ->
    let r = ring_of_thread t thread |> Option.get in
    Ring.push r timestamp (
      match phase with
      | "suspend-domain"
      | "major_gc_stw"
      | "major_gc_phase_change"
      | "stw_api_barrier"
      | "minor_leave_barrier"
      | "stw_leader" -> Ring.Suspend phase
      | _ -> Ring.Gc phase
    )
  | "eio", "suspend-domain", Duration_end
  | "gc", _, Duration_end ->
    let r = ring_of_thread t thread |> Option.get in
    Ring.pop r timestamp
  | "eio", "domain-spawn", Instant ->
    ring_of_thread t thread |> Option.iter (fun (ring : Ring.t) ->
        let parent = List.assoc_opt "parent" args |> Option.get |> id_of_pointer in
        let root = { Ring.parent = Some (timestamp, parent); cc = None } in
        ring.roots <- root :: ring.roots
      )
  | _ -> ()

let process t reader =
  Read.records reader |> Seq.iter @@ fun (r : Read.record) ->
  (* Fmt.epr "%a@." Read.pp_record r; *)
  match r with
  | Event e -> process_event t e
  | User { id; name; _ } ->
    let id = i64 id in
    let x = get t id in
    x.name <- Some name
  | Scheduling Thread_wakeup { cpu; timestamp; id; args = _ } ->
    set_fiber t cpu timestamp (i64 id)
  | Scheduling Unknown _ -> ()
  | Metadata -> ()
  | Kernel _ -> ()
  | Unknown _ -> ()

let create data =
  let t = {
    start_time = Int64.max_int;
    rings = Rings.empty;
    items = Ids.empty;
  } in
  Eio.Buf_read.parse_string_exn (process t) data;
  t
