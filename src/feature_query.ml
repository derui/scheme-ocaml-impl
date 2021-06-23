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

  let features = [ R7RS; Exact_closed; Exact_complex; IEEE_float; Full_unicode; Ratios; Posix; Windows ]

  let is_feature_symbol s =
    let names = List.map show features in
    List.mem s names

  let of_string v =
    let names = List.map (fun v -> (v, show v)) features in
    let feature = List.find (fun (_, s) -> s = v) names in
    fst feature
end

module Feature_requirement = struct
  type t =
    | And                of t list
    | Or                 of t list
    | Not                of t
    | Library            of Library.name
    | Feature_identifier of Feature_identifier.t
  [@@deriving show]

  let ( ! ) v = Not v

  let library name = Library name

  let feature_identifier ident = Feature_identifier ident
end

(** The module to query a feature of scheme to runtime *)
module type S = sig
  val is_implemented : Feature_identifier.t -> bool
  (** [is_implemented feature] returns a feature identifier supported by a runtime or not. *)
end
