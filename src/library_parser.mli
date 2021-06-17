(** The type of export spec *)
module Export_spec : sig
  type t =
    | Ident  of string
    | Rename of string * string

  val show : t -> string
end

(** The type of library declaration *)
module Library_declaration : sig
  type t = {
    export_declaration : Export_spec.t list;
    import_declaration : Import.Import_declaration.t list;
    begin_declaration : Type.data list;
    include_declaration : string list;
    include_ci_declaration : string list;
  }

  val show : t -> string

  val pp : Format.formatter -> t -> unit

  val empty : t
end

val parse : Type.data -> (Library_declaration.t, Type.scheme_error) result
(** [parse data] parse data to return library declaration. *)
