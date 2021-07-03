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

val import :
  env:Type.data Environment.t ->
  declaration:Import_declaration.t ->
  runtime:(module Runtime.S) ->
  Type.data Environment.t Type.evaluation_result
(** [import ~env ~declaration ~runtime] import library from [runtime] into [env] declared by [declaration] *)
