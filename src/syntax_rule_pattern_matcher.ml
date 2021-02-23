module T = Type
module Pr = Printer

module Literal_set = Set.Make (struct
  type t = string

  let compare = Stdlib.compare
end)

open Syntax_rule_lib

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
