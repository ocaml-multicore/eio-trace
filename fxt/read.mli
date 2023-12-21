(** Read files in Fuchsia trace format: https://fuchsia.dev/fuchsia-src/reference/tracing/trace-format *)

type thread = { pid : int64; tid : int64 }

module Args : sig
  type value = [
    | `Int64 of int64
    | `Uint64 of int64
    | `String of string
    | `Pointer of int64
    | `Koid of int64
    | `Unknown of int
  ]

  type t = (string * value) list
end

module Event : sig
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
end

module User : sig
  type t = {
    id : int64;
    name : string;
    thread : thread;
    args : Args.t;
  }
end

module Kernel : sig
  type t = {
    ty : int;
    koid : int64;
    name : string;
    args : Args.t;
  }
end

module Scheduling : sig
  type t =
    | Thread_wakeup of { cpu : int; timestamp : int64; id : int64; args : Args.t }
    | Unknown of int
end

type record =
  | Metadata
  | Event of Event.t
  | User of User.t
  | Kernel of Kernel.t
  | Scheduling of Scheduling.t
  | Unknown of int

val records : record Seq.t Eio.Buf_read.parser

val pp_record : record Fmt.t
