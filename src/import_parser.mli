(** import declaration parser *)

val import_declaration : Import.Import_declaration.t List_parser.t
(** combinator for import declaration *)

val parse : Type.data -> Import.Import_declaration.t Type.evaluation_result
(** Parse a list as import declaration *)
