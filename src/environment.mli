type binding =
  | Value        of Syntax.data
  | Special_form of Types.special_form_evaluator

type t
(** The type of environment *)

val make : ?parent_env:t -> (string * binding) list -> t
(** make a new environment with bindings and [parent_env] if exists. *)

val set : t -> key:string -> v:binding -> Syntax.data Types.evaluation_result
(** [set t ~key ~v] set [v] to [key]. This function behave to replace current value of [key]. If [key] is not defined in
    whole environment, raise error. *)

val get : t -> key:string -> binding Types.evaluation_result
(** [get t ~key] get the binding that bounded by [key]. If [key] is not defined in whole environment, raise error. *)
