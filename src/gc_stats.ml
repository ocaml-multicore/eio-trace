open Eio.Std

module Read = Fxt.Read
module Rings = Map.Make(Int)

let ( ++ ) = Int64.add
let ( -- ) = Int64.sub

type ring_stats = {
  mutable last_event : int64;
  mutable gc_depth : int;
  mutable app_time : int64;
  mutable gc_time : int64;
}

type stats = {
  mutable rings : ring_stats Rings.t;
}

let pp_header f =
  Fmt.pf f "Ring   GC/s     App/s    Total/s   %%GC@,"

let pp_times f ~gc ~app =
  let gc = Int64.to_float gc /. 1e9 in
  let app = Int64.to_float app /. 1e9 in
  Fmt.pf f "%8.3f %8.3f %8.3f %8.2f" gc app (gc +. app)
    (100. *. (gc /. (gc +. app)))

let pp_ring f (id, stats) =
  Fmt.pf f "%3d " id; pp_times f ~gc:stats.gc_time ~app:stats.app_time

let pp_stats f { rings } =
  pp_header f;
  Fmt.(list ~sep:cut) pp_ring f (Rings.bindings rings);
  let gc_time = Rings.fold (fun _ ring acc -> acc ++ ring.gc_time) rings 0L in
  let app_time = Rings.fold (fun _ ring acc -> acc ++ ring.app_time) rings 0L in
  Fmt.pf f "@,@,All "; pp_times f ~gc:gc_time ~app:app_time

let i64 = Int64.to_int

let get_ring ~timestamp t ring =
  match Rings.find_opt ring t.rings with
  | Some x -> x
  | None ->
    let x = { last_event = timestamp; gc_time = 0L; app_time = 0L; gc_depth = 0 } in
    t.rings <- Rings.add ring x t.rings;
    x

let ring_of_thread ~timestamp t (thread : Read.thread) =
  let id = i64 thread.tid in
  if id land 3 = 1 then (
    let ring = id lsr 2 in
    Some (get_ring t ring ~timestamp)
  ) else None

let process_event stats e =
  let { Read.Event.ty; timestamp; thread; category; name = _; args = _ } = e in
  match ring_of_thread stats thread ~timestamp with
  | None -> ()
  | Some ring ->
    let used = timestamp -- ring.last_event in
    if ring.gc_depth = 0 then
      ring.app_time <- ring.app_time ++ used
    else
      ring.gc_time <- ring.gc_time ++ used;
    ring.last_event <- timestamp;
    match category, ty with
    | "gc", Duration_begin -> ring.gc_depth <- ring.gc_depth + 1
    | "gc", Duration_end ->
      (* todo: we could notice if the event is a top-level one and
         reset the depth to 0 in that case, to cope with missed events. *)
      if ring.gc_depth > 0 then
        ring.gc_depth <- ring.gc_depth - 1
      else
        traceln "Warning: unexpected GC end event."
    | _ -> ()

let analyse reader =
  let stats = { rings = Rings.empty } in
  Read.records reader |> Seq.iter (fun (r : Read.record) ->
      match r with
      | Event e -> process_event stats e
      | User _ -> ()
      | Scheduling _ -> ()
      | Metadata -> ()
      | Kernel _ -> ()
      | Unknown _ -> ()
    );
  stats

let main out tracefiles =
  tracefiles |> List.iter (fun tracefile ->
      Eio.Path.with_open_in tracefile @@
      Eio.Buf_read.parse_exn ~max_size:max_int @@ fun r ->
      let stats = analyse r in
      Fmt.pf out "@[<v>%s:@,@,%a@]@." (Eio.Path.native_exn tracefile) pp_stats stats;
      Fmt.pf out "@.Note: all times are wall-clock and so include time spent blocking.@.";
    );
  Ok ()
