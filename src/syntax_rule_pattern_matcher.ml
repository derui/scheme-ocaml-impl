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
  type matched_data = T.data list

  and level = int

  and t = { symbol_table : (string * level, matched_data) Hashtbl.t }

  let show_mapped_type v = Printf.sprintf "Variable(%s)" (List.map Pr.show v |> String.concat ",")

  let show t =
    let show_symbol_table t =
      Hashtbl.to_seq t
      |> Seq.map (fun ((k, level), v) -> Printf.sprintf "%s(%d) => %s" k level (show_mapped_type v))
      |> List.of_seq |> String.concat ";"
    in
    Printf.sprintf "(symbols = %s)" (show_symbol_table t.symbol_table)

  let pp fmt t = Format.fprintf fmt "%s" @@ show t

  let make () = { symbol_table = Hashtbl.create 0 }

  let get_pattern_variable k t = Hashtbl.find_opt t.symbol_table k |> Option.map List.rev

  let put_pattern_variable key data t =
    (match Hashtbl.find_opt t.symbol_table key with
    | None       -> Hashtbl.replace t.symbol_table key [ data ]
    | Some data' -> Hashtbl.replace t.symbol_table key (data :: data'));
    t

  let rec match_pattern pattern datum literal_set level t =
    match (pattern, datum) with
    | Pattern.Symbol v, T.Symbol s when Literal_set.mem v literal_set && v = s -> Some t
    (* match, but can not use matched value in template *)
    | Pattern.Symbol v, _ when v = "_" -> Some t
    | Pattern.Symbol v, (_ as s) -> Some (put_pattern_variable (v, level) s t)
    | Pattern.Constant (T.Number v), T.Number v' when v = v' -> Some t
    | Pattern.Constant T.True, T.True | Pattern.Constant T.False, T.False -> Some t
    | Pattern.Nest patterns, (_ as l) -> match_pattern_list patterns l literal_set level t
    | Pattern.Nest_dot (patterns, dot), (_ as l) -> match_pattern_dot_list (patterns, dot) l literal_set level t
    | Pattern.Nest_ellipsis (patterns, ellipsis_pattern, rest_patterns), (_ as l) ->
        match_pattern_ellipsis_list (patterns, ellipsis_pattern, rest_patterns) l literal_set level t
    | Pattern.Nest_ellipsis_dot (patterns, ellipsis_pattern, rest_patterns, dot), (_ as l) ->
        match_pattern_ellipsis_dot_list (patterns, ellipsis_pattern, rest_patterns, dot) l literal_set level t
    | _ -> None

  and match_pattern_list patterns data literal_set level t =
    let open Lib.Option.Let_syntax in
    match (patterns, data) with
    | [ v ], T.Cons { car = v'; cdr = T.Empty_list } -> match_pattern v v' literal_set level t
    | v :: rest, T.Cons { car = v'; cdr = rest' } ->
        let* t = match_pattern v v' literal_set level t in
        match_pattern_list rest rest' literal_set level t
    | [], T.Empty_list -> Some t
    | _, _ -> None

  and match_pattern_dot_list (patterns, dot) data literal_set level t =
    let open Lib.Option.Let_syntax in
    match (patterns, data) with
    | [ v ], T.Cons { car = v'; cdr = dot' } ->
        let* _ = match_pattern v v' literal_set level t in
        match_pattern dot dot' literal_set level t
    | v :: rest, T.Cons { car = v'; cdr = rest' } ->
        let* _ = match_pattern v v' literal_set level t in
        match_pattern_dot_list (rest, dot) rest' literal_set level t
    | _, _ -> None

  and match_pattern_ellipsis_list (patterns, ellipsis, rest_patterns) data literal_set level t =
    let open Lib.Option.Let_syntax in
    let minimum_len = List.length patterns + List.length rest_patterns
    and list_len = Internal_lib.length_of_list data in
    let* () = if minimum_len > list_len then None else Some () in
    let* t = match_pattern_list patterns (Internal_lib.take_list data (List.length patterns)) literal_set level t in
    let ellipsis_count = max 0 @@ (list_len - minimum_len) in
    let rec match_ellipsis count data t =
      if count = 0 then Some data
      else
        match data with
        | T.Cons { car = v; cdr = rest } ->
            let* t = match_pattern ellipsis v literal_set (succ level) t in
            match_ellipsis (pred count) rest t
        | _                              -> None
    in
    let* data = match_ellipsis ellipsis_count (Internal_lib.tail_list data @@ List.length patterns) t in
    let* t = match_pattern_list rest_patterns data literal_set level t in
    Some t

  and match_pattern_ellipsis_dot_list (patterns, ellipsis, rest_patterns, dot) data literal_set level t =
    let open Lib.Option.Let_syntax in
    let minimum_len = List.length patterns + List.length rest_patterns
    and list_len = Internal_lib.length_of_list data in
    let* () = if minimum_len > list_len then None else Some () in
    let* t = match_pattern_list patterns (Internal_lib.take_list data (List.length patterns)) literal_set level t in
    let ellipsis_count = max 0 @@ (list_len - minimum_len) in
    let rec match_ellipsis count data t =
      if count = 0 then Some t
      else
        match data with
        | T.Cons { car = v; cdr = rest } ->
            let* t = match_pattern ellipsis v literal_set (succ level) t in
            match_ellipsis (pred count) rest t
        | _                              -> None
    in
    let* t = match_ellipsis ellipsis_count (Internal_lib.tail_list data @@ List.length patterns) t in
    let* t =
      match_pattern_dot_list (rest_patterns, dot)
        (Internal_lib.tail_list data (List.length patterns + ellipsis_count))
        literal_set level t
    in
    Some t

  (* Get mapped symbol table from list by the pattern if matched. *)
  let match_syntax_rule ~syntax_rule ~literals ~data =
    let pattern = Syntax_rule_lib.Syntax_rule.pattern syntax_rule in
    let t = make () in
    let literals = Literal_set.of_list literals in
    match_pattern pattern data literals 0 t
end

let match_syntax_rules ~syntax_rules ~data =
  let module L = Syntax_rule_lib in
  let { L.Syntax_rules.literals; syntax_rules; _ } = syntax_rules in
  let result =
    List.fold_left
      (fun ret syntax_rule ->
        match ret with
        | Some _ -> ret
        | None   ->
            let v = Pattern_matcher.match_syntax_rule ~syntax_rule ~literals ~data in
            v |> Option.map (fun v -> (v, syntax_rule)))
      None syntax_rules
  in
  result
