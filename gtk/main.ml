open Eio_trace

let (_ : string) = GMain.init ()

let load tracefile =
  let ch = open_in_bin tracefile in
  let len = in_channel_length ch in
  let data = really_input_string ch len in
  close_in ch;
  let trace = Trace.create data in
  Model.of_trace trace

let show ?args tracefile =
  let title =
    match args with
    | None ->
      Printf.sprintf "%s (%s) - eio-trace"
        (Filename.basename tracefile)
        (Filename.dirname tracefile)
    | Some args ->
      String.concat " " args
  in
  Gtk_ui.create ~title (load tracefile)

let render ~output tracefile =
  let m = load (tracefile) in
  let v =
    View.of_model m
      ~width:1280.
      ~height:((float m.height +. 0.5) *. View.pixels_per_row +. 2. *. View.v_margin)
  in
  View.zoom_to_fit v;
  let surface = Cairo.SVG.create
    output
    ~w:v.width
    ~h:v.height
  in
  let cr = Cairo.create surface in
  Cairo.rectangle cr 0.0 0.0 ~w:v.width ~h:v.height;
  Cairo.clip cr;
  Render_cairo.render v cr;
  Cairo.Surface.finish surface

let () =
  match Array.to_list Sys.argv with
  | _ :: "show" :: tracefiles -> List.iter show tracefiles; GMain.main ()
  | _ :: "run" :: tracefile :: args -> show ~args tracefile; GMain.main ()
  | [ _; "render-svg"; tracefile; output ] -> render tracefile ~output
  | _ -> failwith "Invalid arguments (eio-trace-gtk should be run via eio-trace)"
