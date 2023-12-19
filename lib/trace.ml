module Domains = Map.Make(Int)
module Ids = Map.Make(Int)

type timestamp = Runtime_events.Timestamp.t
  
type event =
  | Log of string
  | Create_cc of Eio_runtime_events.cc_ty * item
  | Add_fiber of item
and item = {
  id : int;
  mutable parent : item option;
  mutable events : (timestamp * event) list;
  mutable inner_cc : int;
  mutable end_time : timestamp option;
  mutable activations : (timestamp * [`Span of string | `Suspend of string] list) list
}

type t = {
  mutable root : (timestamp * item) option;
  mutable current_fiber : int Domains.t;        (* Head of its activation is a resume *)
  mutable items : item Ids.t;
}

let create () = {
  root = None;
  current_fiber = Domains.empty;
  items = Ids.empty;
}

let current_fiber t ring =
  Domains.find_opt ring t.current_fiber
  |> Option.map (fun id -> Ids.find id t.items)

let current_item t ring =
  Domains.find_opt ring t.current_fiber
  |> Option.map (fun id ->
      let cc = (Ids.find id t.items).inner_cc in
      Ids.find cc t.items
    )

let get t id =
  match Ids.find_opt id t.items with
  | Some x -> x
  | None ->
    let x = { id; events = []; activations = []; inner_cc = id; parent = None; end_time = None } in
    t.items <- Ids.add id x t.items;
    x

let get_stack fiber =
  match fiber.activations with
  | [] -> []
  | (_, x) :: _ -> x

let pp_item f x = Fmt.int f x.id

let is_running x =
  match x.activations with
  | (_, `Suspend _ :: _) :: _ -> false
  | _ -> true

let set_fiber t ring ts id =
  let run () =
    t.current_fiber <- Domains.add ring id t.current_fiber;
    let f = get t id in
    let stack =
      match get_stack f with
      | `Suspend _ :: xs -> xs
      | xs -> xs
    in
    f.activations <- (ts, stack) :: f.activations
  in
  match current_fiber t ring with
  | Some old when old.id = id && is_running old -> ()
  | Some old when is_running old ->
    let stack = `Suspend "" :: get_stack old in
    old.activations <- (ts, stack) :: old.activations; run ()
  | _ -> run ()

let callbacks t =
  Runtime_events.Callbacks.create ()
    (* Uncomment to trace GC events too: *)
(*
      ~runtime_begin:(handle (fun f phase -> Fmt.pf f "begin %s" (Runtime_events.runtime_phase_name phase)))
      ~runtime_end:(handle (fun f phase -> Fmt.pf f "end %s" (Runtime_events.runtime_phase_name phase)))
*)
    ~lost_events:(fun ring n -> Fmt.epr "Warning: ring %d lost %d events@." ring n)
  |> Eio_runtime_events.add_callbacks
    (fun ring ts e ->
       Fmt.epr "%a@." Eio_runtime_events.pp_event e;
       match e with
       | `Fiber id -> set_fiber t ring ts id
       | `Create (id, detail) ->
         let x = get t id in
         begin match detail with
           | `Fiber_in cc ->
             let cc = Ids.find cc t.items in
             x.parent <- Some cc;
             set_fiber t ring ts id;
             cc.events <- (ts, Add_fiber x) :: cc.events;
           | `Cc ty ->
             let current = current_fiber t ring in
             if t.root = None then t.root <- Some (ts, x)
             else (
               let fiber = Option.get current in
               let parent = get t fiber.inner_cc in
               x.parent <- Some parent;
               fiber.inner_cc <- id;
               parent.events <- (ts, Create_cc (ty, x)) :: parent.events;
             )
           | _ -> ()
         end
       | `Exit_cc ->
         let fiber = Option.get (current_fiber t ring) in
         let item = get t fiber.inner_cc in
         item.end_time <- Some ts;
         fiber.inner_cc <- (Option.get item.parent).id;
       | `Exit_fiber id ->
         let item = get t id in
         item.end_time <- Some ts;
         begin
           match current_fiber t ring with
           | Some f when f.id = id ->
             t.current_fiber <- Domains.remove ring t.current_fiber;
             f.activations <- (ts, []) :: f.activations
           | _ -> ()
         end
       | `Log msg ->
         current_item t ring |> Option.iter @@ fun item ->
         item.events <- (ts, Log msg) :: item.events
       | `Name (id, name) ->
         let item = get t id in
         item.events <- (ts, Log name) :: item.events
       | `Enter_span op ->
         current_fiber t ring |> Option.iter @@ fun fiber ->
         let stack = `Span op :: get_stack fiber in
         fiber.activations <- (ts, stack) :: fiber.activations
       | `Exit_span ->
         current_fiber t ring |> Option.iter @@ fun fiber ->
         let stack =
           match get_stack fiber with
           | `Span _ :: xs -> xs
           | xs -> xs
         in
         fiber.activations <- (ts, stack) :: fiber.activations
       | `Suspend_fiber op ->
         current_fiber t ring |> Option.iter @@ fun fiber ->
         let stack = `Suspend op :: get_stack fiber in
         fiber.activations <- (ts, stack) :: fiber.activations
       | _ -> ()
    )
