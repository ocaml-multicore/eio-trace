open Cmdliner

let ( $ ) = Term.app
let ( $$ ) f x = Term.const f $ x

let time =
  let parse s =
    Scanf.sscanf_opt s "%f %s" @@ fun v units ->
    match units with
    | "" -> Ok v
    | "m" -> Ok (v *. 60.)
    | "ms" -> Ok (v /. 1e3)
    | "us" -> Ok (v /. 1e6)
    | "ns" -> Ok (v /. 1e9)
    | x -> Fmt.error "Unknown time unit %S" x
  in
  let parse s = Option.value (parse s) ~default:(Fmt.error "Invalid duration %S" s) in
  let print f x = Fmt.pf f "%fns" x in
  Arg.conv' (parse, print)

let tracefile =
  let doc = "The path of the trace file." in
  Arg.(value @@ opt string "trace.fxt" @@ info ["f"; "tracefile"] ~docv:"PATH" ~doc)

let tracefiles =
  let doc = "The path of the trace file(s)." in
  Arg.(value @@ pos_all string ["trace.fxt"] @@ info [] ~docv:"PATH" ~doc)

let imagefile =
  let doc = "The path of the output image." in
  Arg.(required @@ pos 0 (some string) None @@ info [] ~docv:"OUTPUT" ~doc)

let freq =
  let doc = "How many times per second to check for events." in
  Arg.(value @@ opt float 100.0 @@ info ["F"; "freq"] ~docv:"RATE" ~doc)

let start_time =
  let doc = "Seconds to skip before the section to display." in
  Arg.(value @@ opt (some time) None @@ info ["s"; "start-time"] ~doc)

let duration =
  let doc = "Width of the output image in seconds." in
  Arg.(value @@ opt (some time) None @@ info ["d"; "duration"] ~doc)

let child_args =
  let doc = "The command to be executed and monitored." in
  Arg.(non_empty @@ pos_all string [] @@ info [] ~docv:"command" ~doc)
    
let eio_trace_gtk = "eio-trace-gtk"

let find_eio_trace_gtk () =
  let self = Sys.argv.(0) in
  if Filename.is_implicit self then eio_trace_gtk
  else (
    (* Try to find the GTK binary in our own directory first.
       Useful for testing without installing. *)
    let my_dir = Filename.dirname self in
    let path = Filename.concat my_dir eio_trace_gtk in
    if Sys.file_exists path then path
    else eio_trace_gtk
  )

let exec_gtk args =
  let gtk_exe = find_eio_trace_gtk () in
  try
    Unix.execvp gtk_exe (Array.of_list (gtk_exe :: args))
  with Unix.Unix_error(ENOENT, _, _) ->
    Fmt.error "%S not found; can't run UI" gtk_exe

let show tracefiles = exec_gtk ("show" :: tracefiles)

let run ~fs ~proc_mgr freq args =
  let gtk_exe = find_eio_trace_gtk () in
  let ui tracefile =
    Eio.Process.run proc_mgr (gtk_exe :: "run" :: tracefile :: args);
    Ok ()
  in
  Record.run ~fs ~proc_mgr ~freq ~ui args

let record ~fs ~proc_mgr freq tracefile args =
  Record.run ~fs ~proc_mgr ~freq ~tracefile args

let render tracefile output start_time duration =
  if Filename.check_suffix output ".svg" then (
    let start_time = Option.value start_time ~default:0.0 |> string_of_float in
    let duration = Option.map string_of_float duration |> Option.value ~default:"" in
    exec_gtk ["render-svg"; tracefile; output; start_time; duration]
  ) else
    Fmt.error "Unknown file extension in %S (should end in e.g. '.svg')" output

let cmd env =
  let fs = Eio.Stdenv.fs env in
  let proc_mgr = Eio.Stdenv.process_mgr env in
  let path x = Eio.Path.( / ) fs $$ x in
  Cmd.group (Cmd.info "eio-trace")
  @@ List.map (fun (name, term) -> Cmd.v (Cmd.info name) term) [
    "record", record ~fs ~proc_mgr           $$ freq $ path tracefile $ child_args;
    "dump",   Dump.main Format.std_formatter $$ path tracefile;
    "run",    run ~fs ~proc_mgr              $$ freq $ child_args;
    "show",   show                           $$ tracefiles;
    "render", render                         $$ tracefile $ imagefile $ start_time $ duration;
  ]

let () =
  Eio_main.run @@ fun env ->
  exit (Cmd.eval_result (cmd env))
