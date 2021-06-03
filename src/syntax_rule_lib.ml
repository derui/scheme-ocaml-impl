module T = Type
module Pr = Printer

module Literal_set = Set.Make (struct
  type t = string

  let compare = Stdlib.compare
end)

(* module to parse scheme's list as data type defined in OCaml. This module define some parser combinator to be used in
   parsing syntax-rule. *)
module List_parser = struct
  (* The type of parser *)
  type 'a t = T.data -> ('a * T.data, T.scheme_error) result

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
    | T.Empty_list                           -> T.raise_syntax_error "end of list"
    | Cons { car = v; cdr = Cons _ as rest } -> Ok (v, rest)
    | Cons { car = v; cdr = T.Empty_list }   -> Ok (v, Empty_list)
    | Cons { car = v; cdr = k }              -> Ok (v, k)
    | _ as v                                 -> T.raise_syntax_error (Printf.sprintf "malformed list: %s" @@ Pr.print v)

  let cdr = function
    | T.Empty_list -> T.raise_syntax_error "should be end"
    | Cons _       -> T.raise_syntax_error "not malformed list"
    | v            -> Ok (v, T.Empty_list)

  let zero v = T.raise_syntax_error (Printf.sprintf "empty: %s" @@ Pr.print v)

  let choice p q data =
    let p = p data in
    let q = q data in
    match (p, q) with Error _, Error _ -> T.raise_syntax_error "can not choice" | Error _, Ok v | Ok v, _ -> Ok v

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

  type variables = (string list * int) list

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

type template = T.data

module Syntax_rule = struct
  type t = pattern_in_rule * template

  let show (pattern, template) = Printf.sprintf "%s => %s" (Pattern.show pattern) (Pr.print template)

  let pp fmt t = Format.fprintf fmt "%s" @@ show t

  let template (_, v) = v

  let pattern (v, _) = v

  (** get pattern variables in the pattern list. *)
  let pattern_variables literals (pattern, _) =
    let literals = Literal_set.of_list literals in
    let rec pattern_variables' level accum pattern =
      match pattern with
      | Pattern.Symbol s when Literal_set.mem s literals -> accum
      | Symbol s -> (level, s) :: accum
      | Constant _ -> accum
      | Nest list ->
          let v = list |> List.map (pattern_variables' level []) |> List.concat in
          v @ accum
      | Nest_dot (list, dot) ->
          let list = list |> List.map (pattern_variables' level []) |> List.concat
          and pattern = pattern_variables' level [] dot in
          List.concat [ list; pattern; accum ]
      | Nest_ellipsis (patterns, ellipsis, rest_patterns) ->
          let variables = patterns |> List.map (pattern_variables' level []) |> List.concat
          and ellipsis = pattern_variables' (succ level) [] ellipsis
          and rest_variables = rest_patterns |> List.map (pattern_variables' level []) |> List.concat in
          List.concat [ variables; ellipsis; rest_variables ]
      | Nest_ellipsis_dot (patterns, ellipsis, rest_patterns, dot) ->
          let variables = patterns |> List.map (pattern_variables' level []) |> List.concat
          and ellipsis = pattern_variables' (succ level) [] ellipsis
          and rest_variables = rest_patterns |> List.map (pattern_variables' level []) |> List.concat
          and dot = pattern_variables' level [] dot in
          List.concat [ variables; ellipsis; rest_variables; dot ]
    in
    pattern_variables' 0 [] pattern
end

(** Syntax rules type and functions. Syntax_rule is representation that is a pair of pattern and template in
    syntax-rules. *)
module Syntax_rules = struct
  type t = {
    ellipsis : string;
    literals : string list;
    syntax_rules : Syntax_rule.t list;
  }

  let validate_rule_pattern ellipsis rule =
    let open Lib.Result.Let_syntax in
    let ellipsis_count list =
      List.filter (function Pattern.Symbol v when v = ellipsis -> true | _ -> false) list |> List.length
    in
    let rec validate_rule_pattern' = function
      | Pattern.Nest patterns            ->
          let count = ellipsis_count patterns in
          let* _ = if count > 1 then T.raise_error "Ellipsis cound not contains in same pattern" else Ok () in
          List.fold_left
            (fun accum v ->
              let* () = accum in
              validate_rule_pattern' v)
            (Ok ()) patterns
      | Pattern.Nest_dot (patterns, dot) ->
          let count = ellipsis_count patterns in
          let* _ = if count > 1 then T.raise_error "Ellipsis cound not contains in same pattern" else Ok () in
          let* () =
            List.fold_left
              (fun accum v ->
                let* () = accum in
                validate_rule_pattern' v)
              (Ok ()) patterns
          in
          validate_rule_pattern' dot
      | _                                -> Ok ()
    in
    let* () =
      match Syntax_rule.pattern rule with
      | Pattern.Nest _ | Pattern.Nest_dot _ -> Syntax_rule.pattern rule |> validate_rule_pattern'
      | _                                   -> T.raise_error "Pattern must be list"
    in
    Ok rule

  let validate_template literals ellipsis rule =
    let variables = Syntax_rule.pattern_variables literals rule in
    let open Lib.Result.Let_syntax in
    let rec validate_template' level template =
      let variables_in_level =
        List.filter_map (fun (level', symbol) -> if level >= level' then Some symbol else None) variables
      in
      match template with
      | T.Number _ | T.False | T.True -> Ok rule
      | T.Symbol v when v = ellipsis -> T.raise_error "Invalid template: ellipsis used as unaided"
      | T.Symbol v when level > 0 ->
          if List.mem v variables_in_level then Ok rule else T.raise_error (Printf.sprintf "Invalid level: %s" v)
      | T.Symbol _ -> Ok rule
      | T.Cons { car = T.Symbol e; cdr = T.Cons { car = T.Symbol e'; cdr = T.Empty_list } }
        when e = ellipsis && e' = ellipsis ->
          Ok rule
      | T.Cons { car = v; cdr = T.Cons { car = T.Symbol e; cdr = rest } } when e = ellipsis ->
          let* _ = validate_template' (succ level) v in
          validate_template' level rest
      | T.Cons { car = v; cdr = rest } ->
          let* _ = validate_template' level v in
          validate_template' level rest
      | T.Empty_list -> Ok rule
      | _ as v -> T.raise_error (Printf.sprintf "Invalid syntax: %s" @@ Pr.print v)
    in
    validate_template' 0 @@ Syntax_rule.template rule

  let validate_unique_symbol literals rule =
    let rec collect_symbols accum = function
      | Pattern.Symbol v when List.mem v literals -> accum
      | Symbol v -> v :: accum
      | Constant _ -> accum
      | Nest patterns -> List.fold_left collect_symbols accum patterns
      | Nest_dot (patterns, dot) -> List.fold_left collect_symbols accum (dot :: patterns)
      | Nest_ellipsis (patterns, _, rest) -> List.fold_left collect_symbols accum (patterns @ rest)
      | Nest_ellipsis_dot (patterns, _, rest, dot) -> List.fold_left collect_symbols accum (dot :: patterns @ rest)
    in
    let symbols = collect_symbols [] (Syntax_rule.pattern rule) in
    let uniq_symbols = List.sort_uniq String.compare symbols in
    if List.length symbols <> List.length uniq_symbols then
      T.raise_error "Found some pattern variables appears more than once in the definition"
    else Ok rule

  let validate_syntax_rule literals ellipsis rule =
    let open Lib.Result.Let_syntax in
    let* _ = validate_rule_pattern ellipsis rule in
    let* _ = validate_template literals ellipsis rule in
    Ok rule

  let apply_ellipsis_type ellipsis rule =
    let module P = Pattern in
    let pattern = Syntax_rule.pattern rule in
    let rec change_pattern_type accum dot = function
      | p :: P.Symbol v :: rest when v = ellipsis -> (
          match dot with
          | Some dot -> P.Nest_ellipsis_dot (List.rev accum, p, rest, dot)
          | None     -> P.Nest_ellipsis (List.rev accum, p, rest))
      | [] -> P.Nest (List.rev accum)
      | v :: rest -> change_pattern_type (v :: accum) dot rest
    in
    let rec apply_ellipsis_type' pattern =
      match pattern with
      | P.Constant _ | P.Symbol _  -> pattern
      | P.Nest patterns            ->
          let patterns = List.map apply_ellipsis_type' patterns in
          change_pattern_type [] None patterns
      | P.Nest_dot (patterns, dot) ->
          let patterns = List.map apply_ellipsis_type' patterns in
          change_pattern_type [] (Some dot) patterns
      | _                          -> pattern
    in
    let pattern = apply_ellipsis_type' pattern in
    (pattern, Syntax_rule.template rule)

  let make ?(ellipsis = "...") ?(literals = []) ?(syntax_rules = []) () =
    let open Lib.Result.Let_syntax in
    let apply_validation f rules =
      List.fold_left
        (fun accum rule ->
          let* accum = accum in
          let* rule = f rule in
          Ok (rule :: accum))
        (Ok []) rules
      |> Result.map List.rev
    in
    let* syntax_rules = apply_validation (validate_syntax_rule literals ellipsis) syntax_rules in
    let syntax_rules = List.map (apply_ellipsis_type ellipsis) syntax_rules in
    let* syntax_rules = apply_validation (validate_unique_symbol literals) syntax_rules in
    let* syntax_rules = apply_validation (validate_template literals ellipsis) syntax_rules in
    Ok { ellipsis; literals; syntax_rules }

  let show { ellipsis; literals; syntax_rules; _ } =
    let show_syntax_rules (p, t) = Printf.sprintf "(%s ==> %s)" (Pattern.show p) (Pr.print t) in
    Printf.sprintf "{ellipsis = %s; literals = %s; syntax_rules = %s}" ellipsis (String.concat "," literals)
      (String.concat ";" @@ List.map show_syntax_rules syntax_rules)

  let pp fmt v = Format.fprintf fmt "%s" @@ show v
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
      let p1 = (function T.Symbol s -> Pattern.Symbol s | _ -> failwith "Invalid") <$> symbol in
      let p2 = (fun v -> Pattern.Constant v) <$> constant in
      L.(p1 <|> p2) data
    in
    let pattern_2 data =
      let v =
        let* l = list in
        let* patterns = lift L.(many @@ pattern) l in
        let* dot =
          let cdr_p =
            L.(
              cdr >>= fun v ->
              lift pattern (T.cons v T.Empty_list) >>= fun v -> pure (Some v))
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
    let* cdr =
      let cdr_p =
        L.(
          cdr >>= fun v ->
          lift pattern (T.cons v T.Empty_list) >>= fun v -> pure (Some v))
      in
      L.(cdr_p <|> pure None)
    in
    match cdr with None -> L.pure (Pattern.Nest v) | Some e -> L.pure (Pattern.Nest_dot (v, e))

  let syntax_rule : Syntax_rule.t L.t =
    let open L.Let_syntax in
    let* l = list in
    let* pattern = lift pattern_in_rule l in
    let* template = L.element in
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
    let open L.Infix in
    let* ellipsis = L.(ellipsis <|> L.pure None) in
    let* literals = literals in
    let p = list >>= lift syntax_rule in
    let* syntax_rules = L.many1 p in
    Result.fold ~ok:(fun v -> L.pure v) ~error:(fun _ -> L.zero)
    @@ Syntax_rules.make ?ellipsis ~literals ~syntax_rules ()
end
