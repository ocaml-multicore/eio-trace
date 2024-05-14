type t

val create : packing:(GObj.widget -> unit) -> unit -> t

val show : t -> label:string -> value:string -> (string -> unit) -> unit
(** [show t ~label ~value action] shows the mini-buffer with the given label and initial value.
    When the user presses return, [action] is called with the new value. *)

val hide : t -> unit

val is_open : t -> bool
