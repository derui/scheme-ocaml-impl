open Type

val eval : env -> data -> data evaluation_result
(** [eval env data] evaluate [data] on environment [env], and return result of evaluation. *)
