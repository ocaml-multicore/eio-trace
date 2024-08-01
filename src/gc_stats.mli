val main :
  Format.formatter ->
  _ Eio.Path.t list ->
  (unit, 'a) result
