let debug_layout = false

type timestamp = float    (* ns since start_time *)

module Ids = Map.Make(Int)

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
  | Error of string
  | Create_cc of string * item
  | Add_fiber of { parent : int; child : item }
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

module Ring = struct
  type id = Trace.Ring.id

  type root = {
    mutable parent : (timestamp * int) option;
    mutable cc : (timestamp * item) option;
  }

  type t = {
    events : (timestamp * string list) array;
    mutable y : int;
    mutable height : int;
    mutable roots : root list;
  }
end

type t = {
  items : item Ids.t;
  start_time : int64;
  duration : float;
  height : int;
  rings : Ring.t Trace.Rings.t;
}

let get t id = Ids.find_opt id t.items

let map_event f : Trace.event -> event = function
  | Log x -> Log x
  | Error x -> Error x
  | Create_cc (ty, x) -> Create_cc (ty, f x)
  | Add_fiber { parent; child } -> Add_fiber { parent; child = f child }

let dummy_event = 0., Log ""

let get_id args =
  List.assoc_opt "id" args
  |> function
  | Some (`Pointer x) -> Int64.to_int x
  | _ -> failwith "Missing ID pointer"

let as_string = function
  | `String s -> s
  | _ -> failwith "Not a string"

let layout ~duration (ring : Ring.t) =
  let max_y = ref 1 in
  let rec visit ~y (i : item) =
    if debug_layout then Fmt.epr "%d is at %d@." i.id y;
    i.y <- y;
    i.height <- 1;
    i.end_cc_label <- None;
    let intervals = ref [] in
    i.events |> Array.iter (fun (ts, e) ->
        match e with
        | Log _ | Error _ ->
          if i.end_cc_label = None then (
              i.end_cc_label <- Some ts;
            )
        | Add_fiber _ -> ()
        | Create_cc (_, child) ->
          if debug_layout then Fmt.epr "%d creates cc %d (%a)@." i.id child.id Fmt.(option string) child.name;
          if i.end_cc_label = None then (
              i.end_cc_label <- Some ts;
            );
          visit ~y child;
          i.height <- max i.height child.height;
          let stop = Option.value child.end_time ~default:duration in
          intervals := { Itv.value = child; start = ts; stop } :: !intervals;
      );
    if i.end_cc_label = None then i.end_cc_label <- i.end_time;
    let start_fibers = List.length !intervals in
    i.events |> Array.iter (fun (ts, e) ->
        match e with
        | Log _ | Error _ | Create_cc _ -> ()
        | Add_fiber { parent; child } ->
          if debug_layout then Fmt.epr "%d gets fiber %d, created by %d@." i.id child.id parent;
          let stop = Option.value child.end_time ~default:duration in
          intervals := { Itv.value = child; start = ts; stop } :: !intervals;
      );
    let intervals = List.rev !intervals in
    let itv = Itv.create intervals in
    let height = ref i.height in
    intervals |> List.to_seq |> Seq.drop start_fibers |> Seq.iter (fun (interval : _ Itv.interval) ->
        let y = ref (i.y + 1) in
        let adjust other =
          y := max !y (other.y + other.height);
        in
        Itv.iter_overlaps adjust interval.start interval.stop itv;
        let f = interval.Itv.value in
        visit ~y:!y f;
        height := max !height (f.y - i.y + f.height);
      );
    i.height <- !height;
    max_y := max !max_y i.y;
    if debug_layout then Fmt.epr "%d is at %d+%d@." i.id y i.height;
  in
  let visit_domain root =
    root.Ring.cc |> Option.iter @@ fun (_ts, (i : item)) ->
    i.y <- ring.y + 1;
    i.height <- 1;
    i.end_cc_label <- None;
    i.events |> Array.iter (fun (_ts, e) ->
        match e with
        | Log _ | Error _ | Create_cc _ -> ()
        | Add_fiber { parent = _; child } ->
          visit ~y:(ring.y + i.height) child;
          i.height <- child.y - ring.y + child.height;
          if i.end_cc_label = None then (
            i.end_cc_label <- child.end_cc_label;
          );
      );
  in
  List.iter visit_domain ring.roots;
  ring.height <- (!max_y + 1) - ring.y

let of_trace (trace : Trace.t) =
  let start_time = trace.start_time in
  let duration = ref 0.0 in
  let time ts =
    let f = Int64.sub ts start_time |> Int64.to_float in
    duration := max !duration f;
    f
  in
  let items = ref Ids.empty in
  let rec import (item : Trace.item) =
    let events = import_events item.events in
    let activations = import_activations item.activations in
    let end_time = Option.map time item.end_time in
    let x = { id = item.id; name = item.name; end_time; events; activations; y = 0; height = 0; end_cc_label = None } in
    items := Ids.add x.id x !items;
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
  let import_root { Trace.Ring.parent; cc } =
    {
      Ring.parent = Option.map (fun (ts, id) -> time ts, id) parent;
      cc = Option.map (fun (ts, i) -> time ts, import i) cc;
    }
  in
  let import_ring r =
    let events = List.map (fun (ts, s) -> time ts, s) r.Trace.Ring.events |> List.rev |> Array.of_list in
    let roots = List.map import_root (List.rev r.roots) in
    { Ring.events; y = 0; height = 1; roots }
  in
  let rings = Trace.Rings.map import_ring trace.rings in
  let items = !items in
  let duration = !duration in
  let y = ref 0 in
  rings |> Trace.Rings.iter (fun _ (ring : Ring.t) ->
      ring.y <- !y;
      layout ring ~duration;
      y := !y + ring.height;
    );
  let height = !y in
  { start_time; duration; height; items; rings }

let start_time t = t.start_time

let ring t id = Trace.Rings.find id t.rings

let load tracefile =
  let ch = open_in_bin tracefile in
  let len = in_channel_length ch in
  let data = really_input_string ch len in
  close_in ch;
  let trace = Trace.create data in
  of_trace trace
