open Cmdliner

let ( $ ) = Term.app
let ( $$ ) f x = Term.const f $ x

let tracefile =
  let doc = "The path of the trace file." in
  Arg.(value @@ opt string "trace.fxt" @@ info ["f"; "tracefile"] ~docv:"PATH" ~doc)

let imagefile =
  let doc = "The path of the output image." in
  Arg.(required @@ pos 0 (some string) None @@ info [] ~docv:"OUTPUT" ~doc)

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

let show tracefile = exec_gtk ["show"; tracefile]

let run ~fs ~proc_mgr args =
  let gtk_exe = find_eio_trace_gtk () in
  let ui tracefile =
    Eio.Process.run proc_mgr [gtk_exe; "show"; tracefile];
    Ok ()
  in
  Record.run ~fs ~proc_mgr ~ui args

let record ~fs ~proc_mgr tracefile args =
  Record.run ~fs ~proc_mgr ~tracefile args

let render tracefile output =
  if Filename.check_suffix output ".svg" then
    exec_gtk ["render-svg"; tracefile; output]
  else
    Fmt.error "Unknown file extension in %S (should end in e.g. '.svg')" output

let cmd env =
  let fs = Eio.Stdenv.fs env in
  let proc_mgr = Eio.Stdenv.process_mgr env in
  let path x = Eio.Path.( / ) fs $$ x in
  Cmd.group (Cmd.info "eio-trace")
  @@ List.map (fun (name, term) -> Cmd.v (Cmd.info name) term) [
    "record", record ~fs ~proc_mgr           $$ path tracefile $ child_args;
    "dump",   Dump.main Format.std_formatter $$ path tracefile;
    "run",    run ~fs ~proc_mgr              $$ child_args;
    "show",   show                           $$ tracefile;
    "render", render                         $$ tracefile $ imagefile;
  ]

let () =
  Eio_main.run @@ fun env ->
  exit (Cmd.eval_result (cmd env))
