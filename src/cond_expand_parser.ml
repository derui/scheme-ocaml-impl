module L = List_parser
module T = Type

let symbol = L.satisfy (function T.Symbol _ -> true | _ -> false)

let number = L.satisfy (function T.Number _ -> true | _ -> false)

let pair = L.satisfy (function T.Cons _ -> true | _ -> false)

let name =
  let open L.Let_syntax in
  let* names = pair in
  let p =
    let* idents = L.many1 L.(symbol <|> number) in
    L.pure (List.map Printer.show idents)
  in
  L.nest p names

let rec feature_requirement p =
  let open L.Let_syntax in
  let module F = Feature_query.Feature_identifier in
  let module Fr = Feature_query.Feature_requirement in
  let feature_identifier v =
    let p =
      let open L.Let_syntax in
      let* v = symbol in
      match v with
      | T.Symbol v when F.is_feature_symbol v -> L.pure @@ Fr.feature_identifier @@ F.of_string v
      | _ -> L.zero
    in
    p v
  and library_name v =
    let p =
      let* pair = pair in
      let p' =
        let* _ = L.satisfy (function T.Symbol "library" -> true | _ -> false) in
        let* name = name in
        L.pure @@ Fr.library name
      in
      L.nest p' pair
    in
    p v
  and and_clause v =
    let p =
      let* pair = pair in
      let p' =
        let* _ = L.satisfy (function T.Symbol "and" -> true | _ -> false) in
        let* requirements = L.many feature_requirement in
        L.pure @@ Fr.And requirements
      in
      L.nest p' pair
    in
    p v
  and or_clause v =
    let p =
      let* pair = pair in
      let p' =
        let* _ = L.satisfy (function T.Symbol "or" -> true | _ -> false) in
        let* requirements = L.many feature_requirement in
        L.pure @@ Fr.Or requirements
      in
      L.nest p' pair
    in
    p v
  and not_clause v =
    let p =
      let* pair = pair in
      let p' =
        let* _ = L.satisfy (function T.Symbol "not" -> true | _ -> false) in
        let* requirement = feature_requirement in
        L.pure @@ Fr.Not requirement
      in
      L.nest p' pair
    in
    p v
  in
  L.(feature_identifier <|> library_name <|> and_clause <|> or_clause <|> not_clause) p

let ce_clause v =
  let open L.Let_syntax in
  let module F = Feature_query.Feature_identifier in
  let module Fr = Feature_query.Feature_requirement in
  let p =
    let* pair = pair in
    let p' =
      let* requirement = feature_requirement in
      let* expression = L.element in
      let open Cond_expand in
      L.pure { Cond_expand_clause.feature_requirement = requirement; expression }
    in
    L.nest p' pair
  in
  p v

let else_expression v =
  let open L.Let_syntax in
  let module F = Feature_query.Feature_identifier in
  let module Fr = Feature_query.Feature_requirement in
  let p =
    let* pair = pair in
    let p' =
      let* _ = L.satisfy (function T.Symbol "else" -> true | _ -> false) in
      let* expression = L.element in
      L.pure (Some expression)
    in
    L.nest p' pair
  in
  p v

let parse v =
  let open L.Let_syntax in
  let p =
    let* _ = L.satisfy (function T.Symbol "cond-expand" -> true | _ -> false) in
    let* clauses = L.many ce_clause in
    let* else_expression = L.(else_expression <|> pure None) in
    L.pure { Cond_expand.clauses; else_expression }
  in
  Result.map fst @@ p v
