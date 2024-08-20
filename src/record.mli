type config = {
  freq : float;
  cpu : int option;
  child_args : string list;
}

val run :
  ?ui : (string -> (unit, string) result) ->
  ?tracefile:_ Eio.Path.t ->
  proc_mgr:_ Eio.Process.mgr ->
  fs:_ Eio.Path.t ->
  config ->
  (unit, string) result

val cmdliner : config Cmdliner.Term.t
