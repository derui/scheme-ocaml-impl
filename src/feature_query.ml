module Feature_identifier = struct
  type t =
    | R7RS
    | Exact_complex
    | Exact_closed
    | IEEE_float
    | Full_unicode
    | Ratios
    | Posix
    | Windows
  [@@deriving show]

  let show = function
    | R7RS          -> "r7rs"
    | Exact_closed  -> "exact-closed"
    | Exact_complex -> "exact-complex"
    | IEEE_float    -> "ieee-float"
    | Full_unicode  -> "full-unicode"
    | Ratios        -> "ratios"
    | Posix         -> "posix"
    | Windows       -> "windows"
end

module Feature_requirement = struct
  type t =
    | And                of t * t
    | Or                 of t * t
    | Not                of t
    | Library            of Library.name
    | Feature_identifier of Feature_identifier.t
  [@@deriving show]

  let ( && ) v1 v2 = And (v1, v2)

  let ( || ) v1 v2 = Or (v1, v2)

  let ( ! ) v = Not v

  let library name = Library name

  let feature_identifier ident = Feature_identifier ident
end

(** The module to query a feature of scheme to runtime *)
module type S = sig
  val is_implemented : Feature_identifier.t -> bool
  (** [is_implemented feature] returns a feature identifier supported by a runtime or not. *)
end
