module T = Type
module Pr = Printer

(* module to parse scheme's list as data type defined in OCaml. This module define some parser combinator to be used in
   parsing syntax-rule. *)
module List_parser = struct
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

  open Let_syntax
  open Infix

  (* Apply [p1] and [p2] sequentially and use right result *)
  let ( *> ) p p2 = Infix.((fun _ y -> y) <$> p <*> p2)

  (* Apply [p1] and [p2] sequentially and use left result *)
  let ( *< ) p p2 = Infix.((fun x _ -> x) <$> p <*> p2)

  let element = function
    | T.Empty_list         -> Error "empty list"
    | Cons (v, Empty_list) -> Ok (`Car v, T.Empty_list)
    | Cons (v, rest)       -> Ok (`Car v, rest)
    | _ as v               -> Ok (`Cdr v, T.Empty_list)

  let zero _ = Error "empty"

  let car = function `Car v -> pure v | _ -> zero

  let cdr = function `Cdr v -> pure v | _ -> zero

  let choice p q data =
    let p = p data in
    let q = q data in
    match (p, q) with
    | Error _, Error _              -> Error "can not choice"
    | Error _, Ok v | Ok v, Error _ -> Ok v
    | Ok p, Ok _                    -> Ok p

  (* combinator to choice *)
  let ( <|> ) = choice

  let satisfy p =
    let* v = element in
    if p v then pure v else zero

  let many : 'a t -> 'a list t =
   fun p ->
    let p = (fun v -> [ v ]) <$> p in
    let rec many' accum =
      let* v = p <|> pure [] in
      match v with [] -> List.rev accum |> pure | v :: _ -> many' (v :: accum)
    in
    many' [] <|> pure []

  let many1 p =
    let* p' = p in
    let* ps = many p in
    fun data_list -> Ok (p' :: ps, data_list)

  (* chain one or more repeated operator to result of parser. *)
  let chainl1 : 'a t -> ('a -> 'a -> 'a) t -> 'a t =
   fun p op ->
    let rec chain_rest a =
      let result_of_cycle =
        let* f = op in
        let* v' = p in
        chain_rest (f a v')
      in
      result_of_cycle <|> pure a
    in
    let* a = p in
    chain_rest a

  (* chain zero or more repeated operator to result of parser. *)
  let chainl p op a = chainl1 p op <|> pure a
end

(* module for Pattern and template in rule *)
module Pattern = struct
  type t =
    | Symbol   of string
    | Nested   of t list
    | Constant of T.data
    | Dotted   of t

  let rec show = function
    | Symbol s   -> Printf.sprintf "sym(%s)" s
    | Nested v   -> Printf.sprintf "nested(%s)" @@ String.concat "," @@ List.map show v
    | Constant d -> Printf.sprintf "constant(%s)" @@ Pr.print d
    | Dotted v   -> Printf.sprintf "dotted(%s)" (show v)

  let pp fmt v = Format.fprintf fmt "%s" @@ show v
end

type pattern_in_rule = Pattern.t list

type syntax_rule = pattern_in_rule * Pattern.t

module Syntax_rules = struct
  type t = {
    ellipsis : string;
    literals : string list;
    symbol_table : (string, string) Hashtbl.t;
    patterns : pattern_in_rule list;
  }

  let make () = { ellipsis = "..."; literals = []; symbol_table = Hashtbl.create 0; patterns = [] }
end

(* This module provide some parser combinator to parse expression by rule *)
module Pattern_matcher = struct
  type t = { syntax_rules : Syntax_rules.t }
end

module Rule_parser = struct
  module L = List_parser

  let any_p p = function `Car v | `Cdr v -> p v

  let any = function `Car v | `Cdr v -> L.pure v

  let symbol = L.satisfy @@ any_p T.is_symbol

  let constant = L.(satisfy @@ any_p T.is_number <|> satisfy @@ any_p T.is_true <|> satisfy @@ any_p T.is_false)

  let list = L.satisfy @@ any_p T.is_cons

  let lift p data = match p data with Ok (v, _) -> fun data -> Ok (v, data) | Error _ -> L.zero

  (* parser for pattern. The definition of pattern is required the parser is recursive, so this parser is recursive and
     big. *)
  let rec pattern data =
    let open L.Let_syntax in
    let open L.Infix in
    let pattern_1 data =
      let p =
        (function `Car v -> Pattern.Constant v | `Cdr v -> Pattern.(Dotted (Constant v))) <$> L.(symbol <|> constant)
      in
      p data
    in
    let pattern_2 data =
      let v =
        let* l = list >>= L.car in
        let* nested = lift L.(many @@ pattern) l in
        L.pure (Pattern.Nested nested)
      in
      v data
    (* pattern (<pattern> <pattern> ... . <pattern>) *)
    and pattern_3 data =
      let v =
        let* l = list >>= L.car in
        let* patterns =
          lift
            (let* v = L.many1 @@ pattern in
             let* p2 = L.(element >>= cdr >>= lift pattern) in
             L.pure (v @ [ Pattern.Dotted p2 ]))
            l
        in
        L.pure (Pattern.Nested patterns)
      in
      v data
    in
    L.(pattern_1 <|> pattern_2 <|> pattern_3) data

  let pattern_in_rule : pattern_in_rule L.t =
    let open L.Let_syntax in
    let* v = L.many1 pattern in
    match v with [] -> L.zero | _ :: rest -> L.pure rest

  (* Parsing template is the same rule for pattern. *)
  let template : Pattern.t L.t = pattern

  let syntax_rule : syntax_rule L.t =
    let open L.Let_syntax in
    let open L.Infix in
    let* l = list >>= L.car in
    let* pattern = lift pattern_in_rule l in
    let* template = template in
    L.pure (pattern, template)
end
