module T = Type
module Pr = Printer

type syntax_rule = { symbol_table : (string, string) Hashtbl.t }

type pattern =
  [ `Symbol
  | `Nested     of pattern
  | `Literal    of T.data
  | `Underscore
  ]

(* This module provide some parser combinator to parse expression by rule *)
module Pattern_matcher = struct
  type t = { syntax_rule : syntax_rule }
end

(* module to parse syntax-rules's pattern. This module define some parser combinator to be used in parsing syntax-rule. *)
module Rule_parser = struct
  (* The type of parser *)
  type 'a t = T.data -> ('a * T.data, string) result

  let map : ('a -> 'b) -> 'a t -> 'b t =
   fun f p data -> match p data with Error _ as v -> v | Ok (v, rest) -> Ok (f v, rest)

  let pure : 'a -> 'a t = fun v data -> Ok (v, data)

  let apply fp xp data = match fp data with Error _ as v -> v | Ok (f, rest) -> (map f xp) rest

  let bind : 'a t -> ('a -> 'b t) -> 'b t =
   fun v f data -> match v data with Error _ as e -> e | Ok (v, rest) -> f v rest

  module Infix = struct
    let ( <*> ) = apply

    let ( <$> ) = map

    let ( >>= ) = bind
  end

  module Let_syntax = struct
    let ( let* ) = bind

    let ( let+ ) = apply
  end

  let get_exp = function
    | T.Empty_list         -> Error "empty list"
    | Cons (v, Empty_list) -> Ok (v, T.Empty_list)
    | Cons (v, rest)       -> Ok (v, rest)
    | _ as v               -> Ok (v, T.Empty_list)

  let empty _ = Error "empty"

  let satisfy p data = match p data with Error _ -> Ok (None, data) | Ok (v, _) -> Ok (Some v, data)

  let many : 'a t -> 'a list t =
   fun p ->
    let open Let_syntax in
    let rec many' accum =
      let* v = satisfy p in
      match v with None -> pure (List.rev accum) | Some v -> many' (v :: accum)
    in
    let* ps = many' [] in
    match ps with [] -> pure [] | _ -> pure ps

  let many1 p =
    let open Let_syntax in
    let* p' = p in
    let* ps = many p in
    fun data_list -> Ok (p' :: ps, data_list)
end
