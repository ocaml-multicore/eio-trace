module Itv = Eio_trace.Itv

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

let () =
  Crowbar.(add_test ~name:"ivt" [list span; span] test_ivt)
