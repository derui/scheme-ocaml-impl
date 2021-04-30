type t
(** type of stack of continuation *)

val make : unit -> t
(** [make ()] get a new stack of continuation *)

val push : t -> Continuation.t -> unit
(** [push t continuation] push a continuation to stack. *)

val pop : t -> (Continuation.t, Type.scheme_error) result
(** [pop t] pop a continuation from stack. If stack is empty, return error *)

val replace : t -> t -> unit
(** [replace t other] replace continuations of stack [t] from continuations of [other]. This function allows to change
    call stack. *)
