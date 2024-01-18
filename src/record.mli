val run :
  ?ui : (string -> (unit, string) result) ->
  ?tracefile:_ Eio.Path.t ->
  proc_mgr:_ Eio.Process.mgr ->
  fs:_ Eio.Path.t ->
  string list ->
  (unit, string) result
