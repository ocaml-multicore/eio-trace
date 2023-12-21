module Read = Fxt.Read

let main out tracefile =
  Eio.Path.with_open_in tracefile @@
  Eio.Buf_read.parse_exn ~max_size:max_int @@ fun r ->
  Fmt.pf out "@[<v>%a@]" (Fmt.seq Read.pp_record) (Read.records r)
