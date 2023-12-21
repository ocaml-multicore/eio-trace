module Read = Fxt.Read

module Domains = Map.Make(Int)
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
  | Create_cc of string * item
  | Add_fiber of item
and item = {
  id : int;
  mutable name : string option;
  mutable parent : item option;
  mutable events : (timestamp * event) list;
  mutable inner_cc : int;
  mutable end_time : timestamp option;
  mutable activations : (timestamp * activation) list;
}

module Domain = struct
  type t = {
    mutable current_fiber : int option;
    mutable events : (timestamp * string list) list;
  }
end

type t = {
  mutable root : (timestamp * item) option;
  mutable domains : Domain.t Domains.t;
  mutable items : item Ids.t;
}

let get_domain t ring =
  match Domains.find_opt ring t.domains with
  | Some x -> x
  | None ->
    let x = { Domain.current_fiber = None; events = [] } in
    t.domains <- Domains.add ring x t.domains;
    x

let current_fiber t ring =
  let d = get_domain t ring in
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
  let d = get_domain t ring in
  current_fiber t ring |> Option.iter (fun old -> add_activation old ts `Pause);
  d.current_fiber <- Some id;
  let f = get t id in
  add_activation f ts (`Fiber id)

let id_of_pointer = function
  | `Pointer x -> i64 x
  | _ -> failwith "Not a pointer type!"

let domain_of_thread t (thread : Read.thread) =
  let id = i64 thread.tid in
  if id land 3 = 1 then (
    let ring = id lsr 2 in
    Some (get_domain t ring)
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

let process_event t e =
  let { Read.Event.ty; timestamp; thread; category; name; args } = e in
  match category, name, ty with
  | "eio", "cc", Duration_begin ->
    let id = List.assoc_opt "id" args |> Option.get |> id_of_pointer in
    let ty = List.assoc_opt "type" args |> Option.get |> as_string in
    let cc = get t id in
    fiber_of_thread t thread |> Option.iter (fun parent_fiber ->
        let parent_item = get t (parent_fiber.inner_cc) in
        parent_item.events <- (timestamp, Create_cc (ty, cc)) :: parent_item.events;
        cc.parent <- Some parent_item;
        parent_fiber.inner_cc <- id
      )
  | "eio", "cc", Duration_end ->
    fiber_of_thread t thread |> Option.iter @@ fun fiber ->
    let inner_cc = fiber.inner_cc in
    let cc = get t inner_cc in
    cc.end_time <- Some timestamp;
    cc.parent |> Option.iter (fun parent_item ->
        fiber.inner_cc <- parent_item.id;
      )
  | "eio.span", name, Duration_begin ->
    fiber_of_thread t thread |> Option.iter @@ fun fiber ->
    add_activation fiber timestamp (`Enter_span name)
  | "eio.span", _name, Duration_end ->
    fiber_of_thread t thread |> Option.iter @@ fun fiber ->
    add_activation fiber timestamp `Exit_span
  | "eio", "create-fiber", Instant ->
    let id = List.assoc_opt "id" args |> Option.get |> id_of_pointer in
    let cc = List.assoc_opt "cc" args |> Option.get |> id_of_pointer in
    let x = get t id in
    Ids.find_opt cc t.items |> Option.iter (fun cc ->
        cc.events <- (timestamp, Add_fiber x) :: cc.events
      );
    if t.root = None then t.root <- Some (timestamp, x)
  | "eio", "log", Instant ->
    let msg = List.assoc_opt "message" args |> Option.get |> as_string in
    fiber_of_thread t thread |> Option.iter @@ fun fiber ->
    fiber.events <- (timestamp, Log msg) :: fiber.events
  | "eio", "exit-fiber", Instant ->
    let id = List.assoc_opt "id" args |> Option.get |> id_of_pointer in
    let item = get t id in
    item.end_time <- Some timestamp
  | "eio.suspend", name, Duration_begin ->
    fiber_of_thread t thread |> Option.iter @@ fun fiber ->
    add_activation fiber timestamp (`Suspend_fiber name);
  | "gc", phase, Duration_begin ->
    let d = domain_of_thread t thread |> Option.get in
    let stack =
      match d.events with
      | [] -> []
      | (_, s) :: _ -> s
    in
    d.events <- (timestamp, phase :: stack) :: d.events;
  | "gc", _phase, Duration_end ->
    let d = domain_of_thread t thread |> Option.get in
    let stack =
      match d.events with
      | [] -> []
      | (_, s) :: _ -> s
    in
    d.events <- (timestamp, List.tl stack) :: d.events
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
    root = None;
    domains = Domains.empty;
    items = Ids.empty;
  } in
  Eio.Buf_read.parse_string_exn (process t) data;
  t
