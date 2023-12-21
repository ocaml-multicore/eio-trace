open Eio_trace

let main tracefile =
  let ch = open_in_bin tracefile in
  let len = in_channel_length ch in
  let data = really_input_string ch len in
  close_in ch;
  let trace = Trace.create data in
  Gtk_ui.create (Model.of_trace trace);
  GMain.main ()

open Cmdliner

let tracefile =
  let doc = "The path of the trace file." in
  Arg.(required @@ pos 0 (some string) None @@ info [] ~doc)

let cmd =
  let t = Term.(const main $ tracefile) in
  Cmd.v (Cmd.info "eio-trace-gtk") t

let () = exit (Cmd.eval cmd)
