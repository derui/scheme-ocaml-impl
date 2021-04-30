type t
(** type of continuation *)

val make : body:Type.data -> parent_env:Type.binding Environment.t -> symbol:string -> t
(** [make ~body ~parent_env ~symbol_gen] make a new instance of continuation. *)

val apply :
  t ->
  Type.data ->
  (Type.binding Environment.t -> Type.data -> Type.data Type.evaluation_result) ->
  Type.data Type.evaluation_result
(** [apply t value evaluator] apply continuation with the value. *)
