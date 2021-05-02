module T = Type

type t

val make : T.External_representation.t -> t
(** [make data] make initial context for evaluation *)

val forward : t -> t
(** [forward context] forward location force. *)

val whole_expression : t -> T.External_representation.t

val save_value : t -> T.data -> t
(** [save_value context value] put a value to context. *)

val evaluated_values : t -> T.data list
(** [evaluated_values context] get the list of evaluated value *)
