open Eio_trace

let () = Printexc.record_backtrace true

(* Read trace events from [cursor] until [pid] exits. *)
let collect_events trace cursor pid () =
  try
    let n : int = Runtime_events.read_poll cursor (Trace.callbacks trace) None in
    if n > 0 then true
    else 
    if fst (Unix.waitpid [WNOHANG] pid) = 0 then (
      true
    ) else (
      let _ : int = Runtime_events.read_poll cursor (Trace.callbacks trace) None in
      Gtk_ui.create (Model.of_trace trace);
      false
    )
  with ex ->
    let bt = Printexc.get_raw_backtrace () in
    Fmt.epr "collect_events: %a@." Fmt.exn_backtrace (ex, bt);
    false

let spawn_child ~tmp_dir args =
  let env =
    Array.append
      [|
        "OCAML_RUNTIME_EVENTS_START=1";
        "OCAML_RUNTIME_EVENTS_DIR=" ^ tmp_dir;
        "OCAML_RUNTIME_EVENTS_PRESERVE=1";
      |]
      (Unix.environment ())
  in
  Unix.create_process_env (List.hd args) (Array.of_list args) env Unix.stdin Unix.stdout Unix.stderr

let rec get_cursor tmp_dir pid =
  Unix.sleepf 0.1;
  try Runtime_events.create_cursor (Some (tmp_dir, pid))
  with Failure msg ->
    Fmt.epr "%s (will retry)@." msg;
    get_cursor tmp_dir pid

let main args =
  let tmp_dir = Filename.temp_dir "eio-trace-" ".tmp" in
  let child = spawn_child ~tmp_dir args in
  let cursor = get_cursor tmp_dir child in
  let trace = Trace.create () in
  let _timer = Glib.Timeout.add ~ms:10 ~callback:(collect_events trace cursor child) in
  Fun.protect GMain.main
    ~finally:(fun () ->
        let pid = Unix.create_process "rm" [| "rm"; "-r"; "--"; tmp_dir |] Unix.stdin Unix.stdout Unix.stderr in
        let _, status = Unix.waitpid [] pid in
        assert (status = Unix.WEXITED 0)
      )

open Cmdliner

let child_args =
  let doc = "The command to be executed and monitored." in
  Arg.(non_empty @@ pos_all string [] @@ info [] ~docv:"command" ~doc)

let cmd =
  let t = Term.(const main $ child_args) in
  Cmd.v (Cmd.info "eio-trace") t

let () = exit (Cmd.eval cmd)
