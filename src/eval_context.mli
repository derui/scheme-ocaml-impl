(** Module for evaluation context.

    Evaluation context is *mutable* while all expression is not evaluated. *)

type t
(** type of evaluation context *)

val make : Type.data -> t
(** [make data] make initial context for evaluation *)

val clone : t -> t
(** [clone t] get completely new instance of [t]. *)

val current : t -> Type.data option
(** [current context] get the expression indicated by current indicator.

    Return None if no expression rest at indicated. *)

val push_value : t -> value:Type.data -> t
(** [store_value context value] put a value to context. This function forward indicator.*)

val evaluated_values : t -> Type.data list
(** [evaluated_values context] get the list of evaluated value *)
