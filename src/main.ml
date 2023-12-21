open Cmdliner

let ( $ ) = Term.app
let ( $$ ) f x = Term.const f $ x

let tracefile =
  let doc = "The path of the trace file." in
  Arg.(value @@ opt string "trace.fxt" @@ info ["f"; "tracefile"] ~docv:"PATH" ~doc)

let child_args =
  let doc = "The command to be executed and monitored." in
  Arg.(non_empty @@ pos_all string [] @@ info [] ~docv:"command" ~doc)

let cmd env =
  let fs = Eio.Stdenv.fs env in
  let proc_mgr = Eio.Stdenv.process_mgr env in
  let path x = Eio.Path.( / ) fs $$ x in
  Cmd.group (Cmd.info "eio-trace")
  @@ List.map (fun (name, term) -> Cmd.v (Cmd.info name) term) [
    "record", Record.main ~fs ~proc_mgr      $$ path tracefile $ child_args;
    "dump",   Dump.main Format.std_formatter $$ path tracefile;
  ]

let () =
  Eio_main.run @@ fun env ->
  exit (Cmd.eval (cmd env))
