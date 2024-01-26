open Eio_trace

let (_ : string) = GMain.init ()

let load tracefile =
  let ch = open_in_bin tracefile in
  let len = in_channel_length ch in
  let data = really_input_string ch len in
  close_in ch;
  let trace = Trace.create data in
  Layout.of_trace trace

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

let render ~output ~start_time ?duration ~format tracefile =
  let l = load (tracefile) in
  let v =
    View.of_layout l
      ~width:1280.
      ~height:((float l.height +. 0.5) *. View.pixels_per_row +. 2. *. View.v_margin)
  in
  View.zoom_to_fit v ~start_time ?duration;
  let create =
    match format with
    | `Svg -> Cairo.SVG.create output
    | `Png -> fun ~w ~h -> Cairo.Image.create RGB24 ~w:(int_of_float w) ~h:(int_of_float h)
  in
  let surface =
    create
    ~w:v.width
    ~h:v.height
  in
  let cr = Cairo.create surface in
  Cairo.rectangle cr 0.0 0.0 ~w:v.width ~h:v.height;
  Cairo.clip cr;
  Render_cairo.render v cr;
  begin match format with
    | `Svg -> ()
    | `Png -> Cairo.PNG.write surface output
  end;
  Cairo.Surface.finish surface

let format_of_string = function
  | ".svg" -> `Svg
  | ".png" -> `Png
  | x -> Fmt.failwith "Unknown format %S (should be .svg or .png)" x

let () =
  match Array.to_list Sys.argv with
  | _ :: "show" :: tracefiles -> List.iter show tracefiles; GMain.main ()
  | _ :: "run" :: tracefile :: args -> show ~args tracefile; GMain.main ()
  | [ _; "render-svg"; tracefile; format; output; start_time; duration ] ->
    let duration =
      match duration with
      | "" -> None
      | x -> Some (float_of_string x *. 1e9)
    in
    render tracefile
      ~output
      ~start_time:(float_of_string start_time *. 1e9)
      ?duration
      ~format:(format_of_string format)
  | args ->
    Fmt.failwith "Invalid arguments (eio-trace-gtk should be run via eio-trace)@.(got %a)"
      Fmt.(Dump.list string) args
