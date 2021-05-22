(** Module for evaluation context.

    Evaluation context is *mutable* while all expression is not evaluated. *)

type t
(** type of evaluation context *)

val make : ?evaluated_values:Type.data -> unit -> t
(** [make ?evaluated_values ()] make initial stack for evaluation. If stack is restored, pass the argument
    evaluated_value. *)

val clone : t -> t
(** [clone t] get completely new instance of [t]. *)

val push_value : t -> value:Type.data -> t
(** [store_value context value] put a value to context. This function forward indicator.*)

val evaluated_values : t -> Type.data
(** [evaluated_values context] get stacked value. This value must be cons. *)
