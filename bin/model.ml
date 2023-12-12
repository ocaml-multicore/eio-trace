type timestamp = float    (* ns since start_time *)

type event =
  | Log of string
  | Create_cc of Eio_runtime_events.cc_ty * item
  | Add_fiber of item
and item = {
  id : int;
  end_time : timestamp option;
  events : (timestamp * event) array;
  mutable y : int;
  mutable height : int;
  mutable end_cc_label : timestamp option;
  mutable activations : (timestamp * timestamp) array;
}

type t = {
  start_time : int64;
  root : item;
}

let map_event f : Trace.event -> event = function
  | Log x -> Log x
  | Create_cc (ty, x) -> Create_cc (ty, f x)
  | Add_fiber x -> Add_fiber (f x)

let dummy_event = 0., Log ""

let of_trace (trace : Trace.t) =
  let start_time, root = Option.get trace.root in
  let start_time = Runtime_events.Timestamp.to_int64 start_time in
  let time ts = Int64.sub (Runtime_events.Timestamp.to_int64 ts) start_time |> Int64.to_float in
  let rec import (item : Trace.item) =
    let events = import_events item.events in
    let activations = import_activations item.activations in
    let end_time = Option.map time item.end_time in
    let x = { id = item.id; end_time; events; activations; y = 0; height = 0; end_cc_label = end_time } in
    x
  and import_activations xs =
    let n_act = List.length xs in
    let arr = Array.make (n_act / 2) (0.0, 0.0) in
    let rec aux i = function
      | [] -> arr
      | t1 :: t0 :: xs ->
        arr.(i) <- (time t0, time t1);
        aux (i - 1) xs
      | [_] -> assert false
    in
    aux (Array.length arr - 1) xs
  and import_events events =
    let a = Array.make (List.length events) dummy_event in
    process a (Array.length a - 1) events;
    a
  and process arr i = function
    | [] -> ()
    | (ts, x) :: xs ->
      arr.(i) <- (time ts, map_event import x);
      process arr (i - 1) xs
  in
  { start_time; root = import root }

let layout t =
  let rec visit ~y (i : item) =
    Fmt.epr "%d is at %d@." i.id y;
    i.y <- y;
    let max_cc_height = ref 1 in
    let have_cc = ref false in
    i.events |> Array.iter (fun (ts, e) ->
        match e with
        | Log _ | Add_fiber _ -> ()
        | Create_cc (_, child) ->
          Fmt.epr "%d creates cc %d@." i.id child.id;
          if not !have_cc then (
              i.end_cc_label <- Some ts;
              have_cc := true
            );
          visit ~y child;
          max_cc_height := max !max_cc_height child.height
      );
    i.height <- !max_cc_height;
    i.events |> Array.iter (fun (_, e) ->
        match e with
        | Log _ | Create_cc _ -> ()
        | Add_fiber f ->
          Fmt.epr "%d creates fiber %d@." i.id f.id;
          visit ~y:(y + i.height) f;
          i.height <- i.height + f.height;
      );
    Fmt.epr "%d is at %d+%d@." i.id y i.height;
  in
  visit t.root ~y:(-1)

let start_time t = t.start_time
