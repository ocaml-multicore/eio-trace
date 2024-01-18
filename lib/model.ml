type timestamp = float    (* ns since start_time *)

module Spans : sig
  type 'a t
  (* The history of a stack of spans of type 'a *)

  val create : unit -> 'a t

  val current : 'a t -> 'a list

  val push : 'a t -> timestamp -> 'a -> unit
  (* [push ts x t] is [t] extended by pushing [x] at time [ts]. *)

  val pop : 'a t -> timestamp -> unit
  (* [pop ts t] is [t] extended by popping a value at time [ts]. *)

  val history : 'a t -> (timestamp * 'a list) list
  (* [history t] is the list of snapshots of [t] from earliest to latest. *)
end = struct
  type 'a t = (timestamp * 'a list) list ref

  let create () = ref []

  let current t =
    match !t with
    | [] -> []
    | (_, xs) :: _ -> xs

  let push t ts x =
    t := (ts, (x :: current t)) :: !t

  let pop t ts =
    let stack =
      match current t with
      | _ :: xs -> xs
      | [] -> []
    in
    t := (ts, stack) :: !t

  let history t = List.rev !t
end

type event =
  | Log of string
  | Create_cc of string * item
  | Add_fiber of item
and item = {
  id : int;
  name : string option;
  end_time : timestamp option;
  events : (timestamp * event) array;
  mutable y : int;
  mutable height : int;
  mutable end_cc_label : timestamp option;
  mutable activations : (timestamp * [ `Span of string | `Suspend of string ] list) array;
}

type t = {
  start_time : int64;
  duration : float;
  root : item;
  height : int;
}

let map_event f : Trace.event -> event = function
  | Log x -> Log x
  | Create_cc (ty, x) -> Create_cc (ty, f x)
  | Add_fiber x -> Add_fiber (f x)

let dummy_event = 0., Log ""

let get_id args =
  List.assoc_opt "id" args
  |> function
  | Some (`Pointer x) -> Int64.to_int x
  | _ -> failwith "Missing ID pointer"

let as_string = function
  | `String s -> s
  | _ -> failwith "Not a string"

let layout root =
  let max_y = ref 1 in
  let rec visit ~y (i : item) =
    Fmt.epr "%d is at %d@." i.id y;
    i.y <- y;
    i.height <- 1;
    i.end_cc_label <- None;
    i.events |> Array.iter (fun (ts, e) ->
        match e with
        | Log _ ->
          if i.end_cc_label = None then (
              i.end_cc_label <- Some ts;
            )
        | Add_fiber _ -> ()
        | Create_cc (_, child) ->
          Fmt.epr "%d creates cc %d (%a)@." i.id child.id Fmt.(option string) child.name;
          if i.end_cc_label = None then (
              i.end_cc_label <- Some ts;
            );
          visit ~y child;
          i.height <- max i.height child.height
      );
    if i.end_cc_label = None then i.end_cc_label <- i.end_time;
    i.events |> Array.iter (fun (_, e) ->
        match e with
        | Log _ | Create_cc _ -> ()
        | Add_fiber f ->
          Fmt.epr "%d creates fiber %d@." i.id f.id;
          visit ~y:(y + i.height) f;
          i.height <- i.height + f.height;
      );
    max_y := max !max_y i.y;
    Fmt.epr "%d is at %d+%d@." i.id y i.height;
  in
  visit root ~y:0;
  !max_y + 1

let of_trace (trace : Trace.t) =
  let start_time, root = Option.get trace.root in
  let duration = ref 0.0 in
  let time ts =
    let f = Int64.sub ts start_time |> Int64.to_float in
    duration := max !duration f;
    f
  in
  let rec import (item : Trace.item) =
    let events = import_events item.events in
    let activations = import_activations item.activations in
    let end_time = Option.map time item.end_time in
    let x = { id = item.id; name = item.name; end_time; events; activations; y = 0; height = 0; end_cc_label = None } in
    x
  and import_activations xs =
    let s = Spans.create () in
    List.rev xs |> List.iter (fun (ts, (e : Trace.activation)) ->
        let ts = time ts in
        match e with
        | `Pause ->
          begin match Spans.current s with
            | `Suspend _ :: _ -> ()
            | _ -> Spans.push s ts (`Suspend "")
          end
        | `Enter_span op ->
          Spans.push s ts (`Span op)
        | `Exit_span ->
          Spans.pop s ts
        | `Fiber _ ->
          begin match Spans.current s with
            | `Suspend _ :: _ -> Spans.pop s ts
            | _ -> ()
          end
        | `Suspend_fiber op -> Spans.push s ts (`Suspend op)
      );
    Array.of_list (Spans.history s)
  and import_events events =
    events |> List.rev |> List.map (fun (ts, x) -> (time ts, map_event import x)) |> Array.of_list
  in
  let root = import root in
  let height = layout root in
  { start_time; duration = !duration; root; height }

let start_time t = t.start_time
