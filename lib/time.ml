let of_string s =
  match
    Scanf.sscanf_opt s "%f %s" @@ fun v units ->
    match units with
    | ""
    | "s" -> Ok v
    | "m" -> Ok (v *. 60.)
    | "ms" -> Ok (v /. 1e3)
    | "us" -> Ok (v /. 1e6)
    | "ns" -> Ok (v /. 1e9)
    | x -> Fmt.error "Unknown time unit %S" x
  with
  | None -> Fmt.error "Invalid duration %S" s
  | Some x -> x

let pp f x =
  if x >= 1.0 then Fmt.pf f "%gs" x
  else if x >= 1e-3 then Fmt.pf f "%gms" (x *. 1e3)
  else if x >= 1e-6 then Fmt.pf f "%gus" (x *. 1e6)
  else Fmt.pf f "%gns" (x *. 1e9)

let to_string = Fmt.to_to_string pp
