(** This module provides type and functions of continuation *)

type 'a t = {
  previous_continuation : 'a t option;
  current_status : 'a;
}
(** The type of continuation. *)

val make : status:'a -> previous_continuation:'a t option -> 'a t
(** [make ~previous_continuation ~status] make new continuation from values. *)
