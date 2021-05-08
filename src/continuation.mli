(** This module provides type and functions of continuation *)

type t = {
  env : Type.binding Environment.t;
  previous_continuation : t option;
  stack : Eval_stack.t;
}
(** The type of continuation. *)

val make : stack:Eval_stack.t -> previous_continuation:t option -> env:Type.binding Environment.t -> t
(** [make ~stack ~previous_continuation ~env] make new continuation from values. *)
