module Itv = Eio_trace.Itv
module Space = Eio_trace.Space

let span = Crowbar.(map [uint8; uint8]) (fun start len -> (float start, float (start + len)))

let itv_span (start, stop) =
  { Itv.start; stop; value = ref false }

let mark x = x := true

let pp_span f (x : _ Itv.interval) =
  Fmt.pf f "[%g, %g)" x.start x.stop

let test_ivt spans (start, stop) =
  let spans = List.map itv_span spans in
  let t = Itv.create spans in
  Itv.iter_overlaps mark start stop t;
  spans |> List.iter (fun span ->
      let overlaps = Itv.overlaps span start stop in
      let reported = !(span.value) in
      if overlaps && not reported then (
        Crowbar.failf "Span %a overlaps [%g, %g) but wasn't returned!@.%a" pp_span span start stop Itv.dump t
      ) else if reported && not overlaps then (
        Crowbar.failf "Span %a doesn't overlap [%g, %g) but was returned!@.%a" pp_span span start stop Itv.dump t
      )
    )

let test_space start used height =
  let s = Space.create start in
  List.iter (Space.mark s) used;
  let free = Space.first_free s height in
  for i = free to free + height - 1 do
    if List.mem i used then
      Crowbar.failf "Row %d is used, but was returned as free (%d+%d)!" i free height
  done

let () =
  Crowbar.(add_test ~name:"ivt" [list span; span] test_ivt);
  Crowbar.(add_test ~name:"space" [int8; list int8; uint8] test_space)
