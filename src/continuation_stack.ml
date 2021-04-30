open Type

type t = Continuation.t list ref

let make () = ref []

(** [push t continuation] push a continuation to the stack. *)
let push t continuation = t := continuation :: !t

(** [pop t] pop a continuation from the stack. *)
let pop t =
  match !t with
  | []        -> raise_error "invalid continuation manipulation"
  | v :: rest ->
      t := rest;
      Ok v

let replace t new_stack = t := !new_stack
