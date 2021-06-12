(** [Import_set] defines import set for import *)
module Import_set : sig
  type rename_pair = {
    from_name : string;
    to_name : string;
  }

  type t =
    | Only         of (t * string list)
    | Except       of (t * string list)
    | Prefix       of (t * string)
    | Rename       of (t * rename_pair list)
    | Library_name of string list

  val show : t -> string
end

(** [Import_declaration] defines declaration of import *)
module Import_declaration : sig
  type t = { import_sets : Import_set.t list }

  val show : t -> string

  val pp : Format.formatter -> t -> unit
end

(** import declaration parser *)
module Parser : sig
  val import_declaration : Import_declaration.t List_parser.t
  (** combinator for import declaration *)

  val parse : Type.data -> Import_declaration.t Type.evaluation_result
  (** Parse a list as import declaration *)
end
