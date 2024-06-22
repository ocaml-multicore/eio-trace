open Eio_trace

let (_ : string) = GMain.init ()

let or_die = function
  | Ok x -> x
  | Error msg -> failwith msg

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
  Gtk_ui.create ~title tracefile

let render ?output ~start_time ?duration ~format tracefile =
  let output =
    match output with
    | Some x -> x
    | None -> Filename.remove_extension tracefile ^ format
  in
  let format = Save.format_of_string format |> or_die in
  let l = Layout.load (tracefile) in
  let v =
    View.of_layout l
      ~width:1280.
      ~height:((float l.height +. 0.5) *. View.pixels_per_row +. 2. *. View.v_margin)
  in
  View.zoom_to_fit v ~start_time ?duration;
  Save.export_image v format output |> or_die;
  Printf.printf "Wrote %S\n" output

let () =
  match Array.to_list Sys.argv with
  | _ :: "show" :: tracefiles -> List.iter show tracefiles; GMain.main ()
  | _ :: "run" :: tracefile :: args -> show ~args tracefile; GMain.main ()
  | _ :: "render-svg" :: format :: output :: start_time :: duration :: tracefiles ->
    let duration =
      match duration with
      | "" -> None
      | x -> Some (float_of_string x *. 1e9)
    in
    let output =
      match output with
      | "" -> None
      | x -> Some x
    in
    let render =
      render
        ?output
        ~start_time:(float_of_string start_time *. 1e9)
        ?duration
        ~format
    in
    List.iter render tracefiles
  | args ->
    Fmt.failwith "Invalid arguments (eio-trace-gtk should be run via eio-trace)@.(got %a)"
      Fmt.(Dump.list string) args
