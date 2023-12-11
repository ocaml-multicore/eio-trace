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

module R = Render.Make(Canvas)

let ( ==> ) signal callback =
  ignore (signal ~callback : GtkSignal.id)

let (_ : string) = GMain.init ()

let create model =
  let window = GWindow.window () in
  window#event#connect#delete ==> (fun _ -> GMain.quit (); true);
  let table = GPack.table ~rows:1 ~columns:1 ~homogeneous:false ~packing:window#add () in
  let area = GMisc.drawing_area ~packing:(table#attach ~left:0 ~top:0 ~expand:`BOTH ~fill:`BOTH) () in
  let v = View.of_model model ~width:1000. ~height:1000. in
  let set_scollbars () = () in
  let redraw () = GtkBase.Widget.queue_draw area#as_widget in
  area#misc#connect#draw ==> (fun cr ->
      let alloc = area#misc#allocation in
      v.width <- float alloc.width;
      v.height <- float alloc.height;
      Cairo.set_font_size cr 16.;
      Cairo.select_font_face cr "Sans";
      Cairo.set_line_join cr Cairo.JOIN_BEVEL;
      R.render v cr;
      true
    );
  area#misc#set_app_paintable true;
  area#event#add [`SCROLL; `BUTTON1_MOTION; `BUTTON_PRESS];
  area#event#connect#scroll ==> (fun ev ->
      let x = GdkEvent.Scroll.x ev in
      let t_at_pointer = View.time_of_x v x in
      let redraw_zoomed () =
        let t_new_at_pointer = View.time_of_x v x in
        View.set_start_time v (v.start_time -. (t_new_at_pointer -. t_at_pointer));
        redraw () in
      begin match GdkEvent.Scroll.direction ev with
        | `UP -> View.zoom v 1.2; set_scollbars (); redraw_zoomed ()
        | `DOWN -> View.zoom v (1. /. 1.2); redraw_zoomed (); set_scollbars ()
        | _ -> () end;
      true
    );

  let drag_start = ref None in
  area#event#connect#button_press ==> (fun ev ->
      match GdkEvent.get_type ev, GdkEvent.Button.button ev with
      | `BUTTON_PRESS, 1 ->
        let start_t = View.time_of_x v (GdkEvent.Button.x ev) in
        drag_start := Some start_t;
        true;
      | _ -> false
    );

  area#event#connect#motion_notify ==> (fun ev ->
      match !drag_start with
      | None -> false
      | Some start_time ->
        let x = GdkEvent.Motion.x ev in
        let time_at_pointer = View.time_of_x v x in
        if time_at_pointer <> start_time then (
          View.set_start_time v (start_time -. View.timespan_of_width v x);
          redraw ()
        );
        true
    );

  window#show ()
