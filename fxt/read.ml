module R = Eio.Buf_read

type thread = { pid : int64; tid : int64 }

type t = {
  strings : (int, string) Hashtbl.t;
  threads : (int, thread) Hashtbl.t;
}

let pp_thread f { pid; tid } = Fmt.pf f "%Ld:%Ld" pid tid

let bad_char = function
  | ' ' | '"' | '\'' | '\\' -> true
  | c ->
    let c = Char.code c in
    c <= 32 || c >= 127

let pp_string f x =
  if x = "" || String.exists bad_char x then Fmt.pf f "%S" x
  else Fmt.string f x

let ( >>> ) = Int64.shift_right_logical
let ( &&& ) = Int64.logand
let i64 = Int64.to_int

let parse_string t id body =
  if id = 0 then (
    "", body
  ) else if id land 0x8000 = 0 then (
    let s = Hashtbl.find_opt t.strings id |> Option.value ~default:"(missing string)" in
    s, body
  ) else (
    let len = id land 0x7fff in
    let s = Cstruct.to_string body ~len in
    let len = ((len + 7) lsr 3) lsl 3 in
    s, Cstruct.shift body len
  )

module Args = struct
  type value = [
    | `Int64 of int64
    | `Uint64 of int64
    | `String of string
    | `Pointer of int64
    | `Koid of int64
    | `Unknown of int
  ]

  type t = (string * value) list

  let parse t cs : (string * value) * Cstruct.t =
    let header = Cstruct.LE.get_uint64 cs 0 in
    let cs = Cstruct.shift cs 8 in
    let ty = i64 header land 0xf in
    let size = i64 (header >>> 4) land 0xfff in
    let name = i64 (header >>> 16) land 0xffff in
    let name, cs = parse_string t name cs in
    let value =
      match ty with
      | 3 -> `Int64 (Cstruct.LE.get_uint64 cs 0)
      | 4 -> `Uint64 (Cstruct.LE.get_uint64 cs 0)
      | 6 ->
        let v = i64 (header >>> 32) land 0xffff in
        let v, _cs = parse_string t v cs in
        `String v
      | 7 -> `Pointer (Cstruct.LE.get_uint64 cs 0)
      | 8 -> `Koid (Cstruct.LE.get_uint64 cs 0)
      | x -> `Unknown x
    in
    let cs = Cstruct.shift cs ((size - 1) * 8) in
    (name, value), cs

  let pp_value f = function
    | `Int64 x -> Fmt.pf f "%Ld" x
    | `Uint64 x -> Fmt.pf f "%Lu" x
    | `String x -> pp_string f x
    | `Koid x -> Fmt.pf f "%Lu" x
    | `Pointer x -> Fmt.pf f "%Lu" x
    | `Unknown x -> Fmt.pf f "(unknown type %d)" x

  let pp f =
    List.iter (fun (k, v) -> Fmt.pf f "@ %a=%a" pp_string k pp_value v)
end

module Event = struct
  type ty =
    | Instant
    | Counter
    | Duration_begin
    | Duration_end
    | Duration_complete
    | Async_begin
    | Async_instant
    | Async_end
    | Flow_begin
    | Flow_step
    | Flow_end
    | Unknown of int

  type t = {
    ty : ty;
    timestamp : Int64.t;
    thread : thread;
    category : string;
    name : string;
    args : Args.t;
  }

  let ty = function
    | 0 -> Instant
    | 1 -> Counter
    | 2 -> Duration_begin
    | 3 -> Duration_end
    | 4 -> Duration_complete
    | 5 -> Async_begin
    | 6 -> Async_instant
    | 7 -> Async_end
    | 8 -> Flow_begin
    | 9 -> Flow_step
    | 10 -> Flow_end
    | x -> Unknown x

  let pp_ty f ty =
    Fmt.string f @@ match ty with
    | Instant -> "instant"
    | Counter -> "counter"
    | Duration_begin -> "duration begin"
    | Duration_end -> "duration end"
    | Duration_complete -> "duration complete"
    | Async_begin -> "async begin"
    | Async_instant -> "async instant"
    | Async_end -> "async end"
    | Flow_begin -> "flow begin"
    | Flow_step  -> "flow step"
    | Flow_end  -> "flow end"
    | Unknown _ -> "UNKNOWN"

  let pp f { ty; timestamp; args; thread; category; name } =
    Fmt.pf f "ts=%Ld %a cat=%a %a %a%a"
      timestamp
      pp_thread thread
      pp_string category
      pp_ty ty
      pp_string name
      Args.pp args
end

module Scheduling = struct
  type t =
    | Thread_wakeup of { cpu : int; timestamp : int64; id : int64; args : Args.t }
    | Unknown of int

  let pp f = function
    | Thread_wakeup { cpu; timestamp; id; args } ->
      Fmt.pf f "thread_wakeup ts=%Ld cpu=%d %Ld%a" timestamp cpu id Args.pp args
    | Unknown x -> Fmt.pf f "unknown %d" x
end

module User = struct
  type t = {
    id : int64;
    name : string;
    thread : thread;
    args : Args.t;
  }

  let pp f { id; name; thread; args } =
    Fmt.pf f "%Ld name=%a thread=%a%a" id pp_string name pp_thread thread Args.pp args
end

module Kernel = struct
  type t = {
    ty : int;
    koid : int64;
    name : string;
    args : Args.t;
  }

  let pp f { ty; koid; name; args } =
    Fmt.pf f "%Ld ty=%d name=%a%a" koid ty pp_string name Args.pp args
end

type record =
  | Metadata
  | Event of Event.t
  | User of User.t
  | Kernel of Kernel.t
  | Scheduling of Scheduling.t
  | Unknown of int

let rec parse_args t n_args body =
  if n_args = 0 then ([], body)
  else (
    let x, body = Args.parse t body in
    let xs, body = parse_args t (n_args - 1) body in
    (x :: xs), body
  )

let scheduling_record ~ty ~value t body =
  match ty with
  | 2 ->
    let n_args = i64 value land 0xf in
    let cpu = i64 (value >>> 4) land 0xffff in
    let timestamp = Cstruct.LE.get_uint64 body 0 in
    let id = Cstruct.LE.get_uint64 body 8 in
    let args, _body = parse_args t n_args (Cstruct.shift body 16) in
    Scheduling.Thread_wakeup { timestamp; cpu; id; args }
  | x -> Unknown x

let unknown_thread = { pid = -1L; tid = -1L }

let parse_thread t thread body =
  if thread = 0 then (
    let pid = Cstruct.LE.get_uint64 body 0 in
    let tid = Cstruct.LE.get_uint64 body 8 in
    { pid; tid }, Cstruct.shift body 16
  ) else (
    let thread = Hashtbl.find_opt t.threads thread |> Option.value ~default:unknown_thread in
    thread, body
  )

let parse_record t ty head body =
  match ty with
  | 0 -> Some Metadata
  | 2 ->
    let idx = i64 head land 0xffff in
    let len = i64 (head >>> 16) land 0xffff in
    let data = Cstruct.to_string body ~len in
    Hashtbl.replace t.strings idx data;
    None
  | 3 ->
    let idx = i64 head land 0xff in
    let pid = Cstruct.LE.get_uint64 body 0 in
    let tid = Cstruct.LE.get_uint64 body 8 in
    Hashtbl.replace t.threads idx { pid; tid };
    None
  | 4 ->
    let ty = i64 head land 0xf in
    let n_args = i64 (head >>> 4) land 0xf in
    let thread = i64 (head >>> 8) land 0xff in
    let category = i64 (head >>> 16) land 0xffff in
    let name = i64 (head >>> 32) land 0xffff in
    let timestamp = Cstruct.LE.get_uint64 body 0 in
    let body = Cstruct.shift body 8 in
    let thread, body = parse_thread t thread body in
    let category, body = parse_string t category body in
    let name, body = parse_string t name body in
    let args, _body = parse_args t n_args body in
    let ty = Event.ty ty in
    Some (Event { ty; timestamp; thread; name; category; args })
  | 6 ->
    let thread = i64 head land 0xff in
    let name = i64 (head >>> 8) land 0xffff in
    let n_args = i64 (head >>> 24) land 0xf in
    let id = Cstruct.LE.get_uint64 body 0 in
    let thread, body = parse_thread t thread body in
    let name, body = parse_string t name (Cstruct.shift body 8) in
    let args, _body = parse_args t n_args body in
    Some (User { id; name; thread; args })
  | 7 ->
    let ty = i64 head land 0xf in
    let name = i64 (head >>> 8) land 0xffff in
    let n_args = i64 (head >>> 24) land 0xf in
    let koid = Cstruct.LE.get_uint64 body 0 in
    let name, body = parse_string t name (Cstruct.shift body 8) in
    let args, _body = parse_args t n_args body in
    Some (Kernel { ty; koid; name; args })
  | 8 ->
    let ty = i64 (head >>> 44) in
    let value = (head &&& 0xfffffffffffL) in
    Some (Scheduling (scheduling_record ~ty ~value t body))
  | x -> Some (Unknown x)

let record t r =
  let header = R.LE.uint64 r in
  let low = (Int64.to_int header land 0xffff) in
  let ty = low land 0xf in
  let payload_size = ((low lsr 4) - 1) * 8 in
  R.ensure r payload_size;
  let x = parse_record t ty (header >>> 16) (Cstruct.sub (R.peek r) 0 payload_size) in
  R.consume r payload_size;
  x

let records r =
  let t = { strings = Hashtbl.create 100; threads = Hashtbl.create 100 } in
  let rec seq () =
    if R.at_end_of_input r then Seq.Nil
    else (
      match record t r with
      | None -> seq ()
      | Some x -> Cons (x, seq)
    )
  in
  seq

let pp_record f = function
  | Metadata -> Fmt.string f "metadata"
  | Event e -> Fmt.pf f "@[<hv2>event %a@]"Event.pp e
  | User x -> Fmt.pf f "@[<hv2>user %a@]" User.pp x
  | Kernel x -> Fmt.pf f "@[<hv2>kernel %a@]" Kernel.pp x
  | Scheduling x -> Fmt.pf f "@[<hv2>scheduling %a@]" Scheduling.pp x
  | Unknown x -> Fmt.pf f "unknown(%d)" x
