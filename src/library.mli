type t
(** the type of library *)

type name = string list [@@deriving show]
(** the type of the name of library *)

val show : t -> string

val pp : Format.formatter -> t -> unit

val make : name -> t
(** [make name] make a new library with [name]. name must be greater than 0 list, and all elements must not be empty
    string. *)

val name : t -> name
(** [name t] get the name of the library [t]. *)

val exports : t -> string list
(** [exports t] get exported symbols from a library [t]. *)

(** Manipulations *)

val export : symbol:string -> ?renamed:string -> t -> t
(** [export ~symbol ?renamed t] export a [symbol] from a library [t]. If [renamed] is specified, a [symbol] is exported
    as [renamed]. *)

val define : symbol:string -> data:Type.data -> t -> t
(** [define ~symbol ~data t] define a [symbol] with [data] to a library [t]. This function overwrite same symbol that if
    it was already defined *)

val as_environment : t -> Type.data Environment.t
(** [as_environment t] get a new [Environment] from a library [t]. A environment returned from this function is
    completely new allocated environment. *)
