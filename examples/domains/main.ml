(* Run jobs using an executor pool, to get a trace with multiple domains showing GC. *)

open Eio.Std

let n_jobs = 10
let domain_count = 2

let run_job () =
  let items = ref [] in
  for _ = 1 to 1000 do
    items := Bytes.create 1000 :: !items
  done

let () =
  Eio_main.run @@ fun env ->
  Switch.run @@ fun sw ->
  let pool = Eio.Executor_pool.create ~sw ~domain_count env#domain_mgr in
  let jobs = List.init n_jobs (fun _ -> Eio.Executor_pool.submit_fork ~sw ~weight:1. pool run_job) in
  List.iter Promise.await_exn jobs
