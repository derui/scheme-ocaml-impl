type t = Type.data

val print : t -> string
(** [print data] get string representation of [data]. If data is constructed from invalid syntax, this function raise
    exception [Failure]. *)

val show : t -> string
(** [show data] this function is alias of [print]. *)

val pp : Format.formatter -> t -> unit
(** [pp fmt data] pretty printer for [t]. *)
