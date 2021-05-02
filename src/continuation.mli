(** This module provides type and functions of continuation *)

type t = {
  env : Type.binding Environment.t;
  previous_continuation : t option;
  context : Eval_context.t;
}
(** The type of continuation. *)

val make : context:Eval_context.t -> previous_continuation:t option -> env:Type.binding Environment.t -> t
(** [make ~context ~previous_continuation ~env] make new continuation from values. *)
