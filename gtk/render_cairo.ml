module Canvas = struct
  include Cairo

  let move_to cr ~x ~y = move_to cr x y
  let line_to cr ~x ~y = line_to cr x y
  let rectangle cr ~x ~y ~w ~h = rectangle cr x y ~w ~h

  let paint_text cr ?clip_area ~x ~y msg =
    match clip_area with
    | None ->
      move_to cr ~x ~y;
      show_text cr msg
    | Some (w, h) ->
      save cr;
      rectangle cr ~x ~y:0.0 ~w ~h;
      clip cr;
      move_to cr ~x ~y;
      show_text cr msg;
      restore cr

  let set_source_rgb cr ~r ~g ~b = set_source_rgb cr r g b
  let set_source_alpha cr ~r ~g ~b a = set_source_rgba cr r g b a
  let set_source_rgba cr ~r ~g ~b ~a = set_source_rgba cr r g b a
end

include Eio_trace.Render.Make(Canvas)
