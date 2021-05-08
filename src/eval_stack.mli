(** Module for evaluation context.

    Evaluation context is *mutable* while all expression is not evaluated. *)

type kind =
  | In_closure
  | In_expression

type t
(** type of evaluation context *)

val make : kind:kind -> Type.data -> t
(** [make data] make initial context for evaluation *)

val kind : t -> kind

val clone : t -> t
(** [clone t] get completely new instance of [t]. *)

val expression : t -> Type.data
(** [expression t] get whole expression of context [t]. *)

val current : t -> Type.data option
(** [current stack] get the expression indicated by current indicator.

    Return None if no expression rest at indicated. *)

val replace_current : t -> Type.data -> t
(** [replace_current t expr] replace current located expression via [expr]. *)

val push_value : t -> value:Type.data -> t
(** [store_value context value] put a value to context. This function forward indicator.*)

val evaluated_values : t -> Type.data
(** [evaluated_values context] get stacked value. This value must be cons. *)
