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
    | T.Empty_list               -> Error "end of list"
    | Cons (v, (Cons _ as rest)) -> Ok (v, rest)
    | Cons (v, T.Empty_list)     -> Ok (v, Empty_list)
    | Cons (v, k)                -> Ok (v, k)
    | _ as v                     -> Error (Printf.sprintf "malformed list: %s" @@ Pr.print v)

  let cdr = function
    | T.Empty_list -> Error "should be end"
    | Cons _       -> Error "not malformed list"
    | v            -> Ok (v, T.Empty_list)

  let zero v = Error (Printf.sprintf "empty: %s" @@ Pr.print v)

  let choice p q data =
    let p = p data in
    let q = q data in
    match (p, q) with Error _, Error _ -> Error "can not choice" | Error _, Ok v | Ok v, _ -> Ok v

  (* combinator to choice *)
  let ( <|> ) = choice

  let tap f data =
    let p =
      let* v = element in
      f v |> pure
    in
    p data |> ignore;
    Ok ((), data)

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

(** module for Pattern in rule.

    This module provides only type. Parser functions are defined in [Rule_parser]. *)
module Pattern = struct
  type t =
    | Symbol            of string
    | Constant          of T.data
    | Nest              of t list
    | Nest_dot          of t list * t
    | Nest_ellipsis     of t list * t * t list
    | Nest_ellipsis_dot of t list * t * t list * t

  let rec show = function
    | Symbol s                              -> Printf.sprintf "%s" s
    | Constant d                            -> Printf.sprintf "%s" @@ Pr.print d
    | Nest v                                -> Printf.sprintf "(%s)" @@ String.concat " " @@ List.map show v
    | Nest_dot (list, v)                    -> Printf.sprintf "(%s . %s)"
                                                 (String.concat "" @@ List.map show list)
                                                 (show v)
    | Nest_ellipsis (list, e, v)            ->
        Printf.sprintf "(%s %s ... %s)"
          (String.concat " " @@ List.map show list)
          (show e)
          (String.concat " " @@ List.map show v)
    | Nest_ellipsis_dot (list, e, list', v) ->
        Printf.sprintf "(%s %s ... %s . %s)"
          (String.concat " " @@ List.map show list)
          (show e)
          (String.concat " " @@ List.map show list')
          (show v)

  let pp fmt v = Format.fprintf fmt "%s" @@ show v
end

type pattern_in_rule = Pattern.t

type syntax_rule = pattern_in_rule * Pattern.t

(** Syntax rules type and functions. Syntax_rule is representation that is a pair of pattern and template in
    syntax-rules. *)
module Syntax_rules = struct
  type t = {
    ellipsis : string;
    literals : string list;
    syntax_rules : syntax_rule list;
  }

  let validate_syntax_rule ellipsis pattern =
    let open Lib.Result.Let_syntax in
    let ellipsis_count list =
      List.filter (function Pattern.Symbol v when v = ellipsis -> true | _ -> false) list |> List.length
    in
    let rec validate_syntax_rule' = function
      | Pattern.Nest patterns            ->
          let count = ellipsis_count patterns in
          let* _ = if count > 1 then Error "Ellipsis cound not contains in same pattern" else Ok () in
          List.fold_left
            (fun accum v ->
              let* () = accum in
              validate_syntax_rule' v)
            (Ok ()) patterns
      | Pattern.Nest_dot (patterns, dot) ->
          let count = ellipsis_count patterns in
          let* _ = if count > 1 then Error "Ellipsis cound not contains in same pattern" else Ok () in
          let* () =
            List.fold_left
              (fun accum v ->
                let* () = accum in
                validate_syntax_rule' v)
              (Ok ()) patterns
          in
          validate_syntax_rule' dot
      | _                                -> Ok ()
    in
    match pattern with
    | Pattern.Nest _ | Pattern.Nest_dot _ -> validate_syntax_rule' pattern
    | _                                   -> Error "Pattern must be list"

  let make ?(ellipsis = "...") ?(literals = []) ?(syntax_rules = []) () =
    let open Lib.Result.Let_syntax in
    let* _ =
      List.fold_left
        (fun accum (patterns, _) ->
          let* _ = accum in
          let* _ = validate_syntax_rule ellipsis patterns in
          Ok ())
        (Ok ()) syntax_rules
    in
    Ok { ellipsis; literals; syntax_rules }

  let show { ellipsis; literals; syntax_rules; _ } =
    let show_syntax_rules (p, t) = Printf.sprintf "(%s ==> %s)" (Pattern.show p) (Pattern.show t) in
    Printf.sprintf "{ellipsis = %s; literals = %s; syntax_rules = %s}" ellipsis (String.concat "," literals)
      (String.concat ";" @@ List.map show_syntax_rules syntax_rules)

  let pp fmt v = Format.fprintf fmt "%s" @@ show v
end

(** Pattern_match do matching between Syntax_rule and list that is argument of syntax. Result of matching is used to
    template expansion in Template_expansion module. *)
module Pattern_matcher = struct
  type mapped_type =
    | Literal  of string
    | Variable of T.data

  and t = {
    symbol_table : (string, mapped_type) Hashtbl.t;
    level : t list;
  }

  let make () = { symbol_table = Hashtbl.create 0; level = [] }

  let set_level t level = { t with level }

  let put_literal t key data =
    Hashtbl.replace t.symbol_table key (Literal data);
    t

  let put_pattern_variable t key data =
    Hashtbl.replace t.symbol_table key (Variable data);
    t

  let rec is_invalid_pattern literals patterns =
    let symbol_patterns =
      List.filter_map (function Pattern.Symbol s -> Some s | _ -> None) patterns
      |> List.filter (fun v -> List.exists (fun l -> l = v) literals)
    in
    let uniq_symbols = List.sort_uniq Stdlib.compare symbol_patterns in
    let nested_patterns = List.filter_map (function Pattern.Nest v -> Some v | _ -> None) patterns in
    if List.length symbol_patterns <> List.length uniq_symbols then true
    else List.exists (is_invalid_pattern literals) nested_patterns

  module Literal_set = Set.Make (struct
    type t = string

    let compare = Stdlib.compare
  end)

  let rec match_pattern pattern datum literal_set t =
    match (pattern, datum) with
    | Pattern.Symbol v, T.Symbol s when Literal_set.mem v literal_set ->
        if v = s then Some (put_literal t v s) else None
    (* match, but can not use matched value in template *)
    | Pattern.Symbol v, (_ as s) when v <> "_" -> Some (put_pattern_variable t v s)
    | Pattern.Symbol v, _ when v = "_" -> Some t
    | Pattern.Constant (T.Number v), T.Number v' when v = v' -> Some t
    | Pattern.Constant T.True, T.True | Pattern.Constant T.False, T.False -> Some t
    | Pattern.Nest patterns, (T.Cons _ as l) -> match_pattern_list patterns l literal_set t
    | Pattern.Nest_dot (patterns, dot), (T.Cons _ as l) -> match_pattern_dot_list (patterns, dot) l literal_set t
    | Pattern.Nest_ellipsis (patterns, ellipsis_pattern, rest_patterns), (T.Cons _ as l) ->
        match_pattern_ellipsis_list (patterns, ellipsis_pattern, rest_patterns) l literal_set t
    | Pattern.Nest_ellipsis_dot (patterns, ellipsis_pattern, rest_patterns, dot), (T.Cons _ as l) ->
        match_pattern_ellipsis_dot_list (patterns, ellipsis_pattern, rest_patterns, dot) l literal_set t
    | _ -> None

  and match_pattern_list patterns data literal_set t =
    let open Lib.Option.Let_syntax in
    match (patterns, data) with
    | [ v ], T.Cons (v', T.Empty_list) -> match_pattern v v' literal_set t
    | v :: rest, T.Cons (v', rest')    ->
        let* _ = match_pattern v v' literal_set t in
        match_pattern_list rest rest' literal_set t
    | [], T.Empty_list                 -> Some t
    | _, _                             -> None

  and match_pattern_dot_list (patterns, dot) data literal_set t =
    let open Lib.Option.Let_syntax in
    match (patterns, data) with
    | [ v ], T.Cons (v', dot')      ->
        let* _ = match_pattern v v' literal_set t in
        match_pattern dot dot' literal_set t
    | v :: rest, T.Cons (v', rest') ->
        let* _ = match_pattern v v' literal_set t in
        match_pattern_dot_list (rest, dot) rest' literal_set t
    | _, _                          -> None

  and match_pattern_ellipsis_list (patterns, ellipsis, rest_patterns) data literal_set t =
    let open Lib.Option.Let_syntax in
    let minimum_len = List.length patterns + List.length rest_patterns
    and list_len = Internal_lib.length_of_list data in
    let* () = if minimum_len > list_len then None else Some () in
    let* t = match_pattern_list patterns (Internal_lib.take_list data (List.length patterns)) literal_set t in
    let ellipsis_count = max 0 @@ (list_len - minimum_len) in
    let rec match_ellipsis count data patterns =
      if count = 0 then Some (List.rev patterns)
      else
        match data with
        | T.Cons (v, rest) ->
            let* t = match_pattern ellipsis v literal_set (make ()) in
            match_ellipsis (pred count) rest (t :: patterns)
        | _                -> None
    in
    let* ellipsis = match_ellipsis ellipsis_count (Internal_lib.tail_list data @@ List.length patterns) [] in
    let leveled_t = set_level t ellipsis in
    let* t =
      match_pattern_list rest_patterns
        (Internal_lib.tail_list data (List.length patterns + ellipsis_count))
        literal_set leveled_t
    in
    Some t

  and match_pattern_ellipsis_dot_list (patterns, ellipsis, rest_patterns, dot) data literal_set t =
    let open Lib.Option.Let_syntax in
    let minimum_len = List.length patterns + List.length rest_patterns
    and list_len = Internal_lib.length_of_list data in
    let* () = if minimum_len > list_len then None else Some () in
    let* t = match_pattern_list patterns (Internal_lib.take_list data (List.length patterns)) literal_set t in
    let ellipsis_count = max 0 @@ (list_len - minimum_len) in
    let rec match_ellipsis count data patterns =
      if count = 0 then Some (List.rev patterns)
      else
        match data with
        | T.Cons (v, rest) ->
            let* t = match_pattern ellipsis v literal_set (make ()) in
            match_ellipsis (pred count) rest (t :: patterns)
        | _                -> None
    in
    let* ellipsis = match_ellipsis ellipsis_count (Internal_lib.tail_list data @@ List.length patterns) [] in
    let leveled_t = set_level t ellipsis in
    let* t =
      match_pattern_dot_list (rest_patterns, dot)
        (Internal_lib.tail_list data (List.length patterns + ellipsis_count))
        literal_set leveled_t
    in
    Some t

  (* Get mapped symbol table from list by the pattern if matched. *)
  let match_rule_pattern ~syntax_rule ~literals _ =
    let patterns, _ = syntax_rule in
    let _ = make () in
    (* TODO: Get more specific error information *)
    if is_invalid_pattern literals patterns then Error "Invalid pattern"
    else
      let _ = Literal_set.of_list literals in
      failwith "not implemented"
end

(** Parser for syntax-rule. *)
module Rule_parser = struct
  module L = List_parser

  let symbol = L.satisfy T.is_symbol

  let constant = L.(satisfy T.is_number <|> satisfy T.is_true <|> satisfy T.is_false)

  let list = L.satisfy @@ function T.Cons _ | T.Empty_list -> true | _ -> false

  let lift p data = match p data with Ok (v, _) -> fun data -> Ok (v, data) | Error _ -> L.zero

  (* parser for pattern. The definition of pattern is required the parser is recursive, so this parser is recursive and
     big. *)
  let rec pattern data =
    let open L.Let_syntax in
    let open L.Infix in
    let pattern_1 data =
      let p = (fun v -> Pattern.Constant v) <$> L.(symbol <|> constant) in
      p data
    in
    let pattern_2 data =
      let v =
        let* l = list in
        let* patterns = lift L.(many @@ pattern) l in
        let* dot =
          let cdr_p =
            L.(
              cdr >>= fun v ->
              lift pattern (T.Cons (v, T.Empty_list)) >>= fun v -> pure (Some v))
          in
          L.(cdr_p <|> pure None)
        in
        match dot with Some dot -> L.pure (Pattern.Nest_dot (patterns, dot)) | None -> L.pure (Pattern.Nest patterns)
      in
      v data
    in
    L.(pattern_1 <|> pattern_2) data

  let pattern_in_rule : pattern_in_rule L.t =
    let open L.Let_syntax in
    let open L.Infix in
    let* _ = symbol in
    let* v = L.many pattern in
    print_endline @@ Printf.sprintf "patterns: (%s)" (String.concat "|" @@ List.map Pattern.show v);
    let* cdr =
      let cdr_p =
        L.(
          cdr >>= fun v ->
          lift pattern (T.Cons (v, T.Empty_list)) >>= fun v -> pure (Some v))
      in
      L.(cdr_p <|> pure None)
    in
    match cdr with None -> L.pure (Pattern.Nest v) | Some e -> L.pure (Pattern.Nest_dot (v, e))

  (* Parsing template is the same rule for pattern. *)
  let template : Pattern.t L.t = pattern

  let syntax_rule : syntax_rule L.t =
    let open L.Let_syntax in
    let* l = list in
    let* pattern = lift pattern_in_rule l in
    let* template = template in
    L.pure (pattern, template)

  let ellipsis =
    let open L.Let_syntax in
    let* symbol = symbol in
    match symbol with T.Symbol v -> L.pure @@ Option.some v | _ -> L.pure None

  let literals =
    let open L.Let_syntax in
    let* l = list in
    let p =
      let* e = L.element in
      match e with T.Symbol v -> L.pure v | _ -> L.zero
    in
    L.(lift (L.many p) l <|> L.pure [])

  let syntax_rules : Syntax_rules.t L.t =
    let open L.Let_syntax in
    let* ellipsis = L.(ellipsis <|> L.pure None) in
    let* literals = literals in
    let p =
      let* l = list in
      lift syntax_rule l
    in
    let* syntax_rules = L.many1 p in
    Result.fold ~ok:(fun v -> L.pure v) ~error:(fun _ -> L.zero)
    @@ Syntax_rules.make ?ellipsis ~literals ~syntax_rules ()
end
