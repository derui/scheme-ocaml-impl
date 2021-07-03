module L = List_parser
module T = Type
module LD = Library_declaration

type declaration =
  | Export                       of LD.Export_spec.t list
  | Import                       of Import.Import_declaration.t
  | Begin                        of T.data list
  | Include                      of string list
  | Include_ci                   of string list
  | Include_library_declarations of string list
  | Cond_expand                  of Cond_expand.t list

let symbol = L.satisfy (function T.Symbol _ -> true | _ -> false)

let number = L.satisfy (function T.Number _ -> true | _ -> false)

let string = L.satisfy (function T.Scheme_string _ -> true | _ -> false)

let pair = L.satisfy (function T.Cons _ -> true | _ -> false)

let identifier =
  let open L.Let_syntax in
  let* v = symbol in
  match v with T.Symbol v -> L.pure v | _ -> L.zero

let name =
  let open L.Let_syntax in
  let* names = pair in
  let p =
    let* idents = L.many1 L.(symbol <|> number) in
    L.pure idents
  in
  L.nest p names

let export_spec =
  let open L.Let_syntax in
  let ident =
    let* v = identifier in
    L.pure (LD.Export_spec.Ident v)
  and rename =
    let* pair = pair in
    let p' =
      let* _ = L.satisfy (function T.Symbol "rename" -> true | _ -> false) in
      let* from_mame = identifier in
      let* to_name = identifier in
      L.pure (LD.Export_spec.Rename (from_mame, to_name))
    in
    L.nest p' pair
  in
  L.(ident <|> rename)

let begin_declaration =
  let open L.Let_syntax in
  let* pair = pair in
  let p =
    let* _ = L.satisfy (function T.Symbol "begin" -> true | _ -> false) in
    let* definitions = L.many L.element in
    L.pure (Begin definitions)
  in
  L.nest p pair

let export_declaration =
  let open L.Let_syntax in
  let* pair = pair in
  let p =
    let* _ = L.satisfy (function T.Symbol "export" -> true | _ -> false) in
    let* specs = L.many export_spec in
    L.pure (Export specs)
  in
  L.nest p pair

let import_declaration =
  let open L.Let_syntax in
  let* pair = pair in
  let p =
    let* import_declaration = Import_parser.import_declaration in
    L.pure (Import import_declaration)
  in
  L.nest p pair

let include_declaration =
  let open L.Let_syntax in
  let* pair = pair in
  let p =
    let* _ = L.satisfy (function T.Symbol "include" -> true | _ -> false) in
    let* include_ = L.many1 string in
    let include_ = List.filter_map (function T.Scheme_string v -> Some v | _ -> None) include_ in
    L.pure
      (Include (List.map (fun chars -> List.map Data_type.Scheme_char.to_string chars |> String.concat "") include_))
  in
  L.nest p pair

let include_ci_declaration =
  let open L.Let_syntax in
  let* pair = pair in
  let p =
    let* _ = L.satisfy (function T.Symbol "include-ci" -> true | _ -> false) in
    let* includes = L.many1 string in
    let includes = List.filter_map (function T.Scheme_string v -> Some v | _ -> None) includes in
    L.pure
      (Include_ci (List.map (fun chars -> List.map Data_type.Scheme_char.to_string chars |> String.concat "") includes))
  in
  L.nest p pair

let include_library_declarations =
  let open L.Let_syntax in
  let* pair = pair in
  let p =
    let* _ = L.satisfy (function T.Symbol "include-library-declarations" -> true | _ -> false) in
    let* includes = L.many1 string in
    let includes = List.filter_map (function T.Scheme_string v -> Some v | _ -> None) includes in
    L.pure
      (Include_library_declarations
         (List.map (fun chars -> List.map Data_type.Scheme_char.to_string chars |> String.concat "") includes))
  in
  L.nest p pair

let cond_expand =
  let open L.Let_syntax in
  let* pair = pair in
  let p =
    let* cond_expand = Cond_expand_parser.cond_expand in
    L.pure @@ Cond_expand [ cond_expand ]
  in
  L.nest p pair

let parse v =
  let open L.Let_syntax in
  let p =
    let* name = name in
    let* declarations =
      L.(
        many
        @@ (export_declaration <|> import_declaration <|> begin_declaration <|> include_declaration
          <|> include_ci_declaration <|> include_library_declarations <|> cond_expand))
    in

    let declaration =
      List.fold_left
        (fun (accum : Library_declaration.t) v ->
          match v with
          | Begin v                        -> {
                                                accum with
                                                begin_declarations = List.concat [ accum.begin_declarations; v ];
                                              }
          | Export v                       -> {
                                                accum with
                                                export_declarations = List.concat [ accum.export_declarations; v ];
                                              }
          | Import v                       -> {
                                                accum with
                                                import_declarations = List.concat [ accum.import_declarations; [ v ] ];
                                              }
          | Include v                      -> {
                                                accum with
                                                include_declarations = List.concat [ accum.include_declarations; v ];
                                              }
          | Include_library_declarations v ->
              { accum with include_library_declarations = List.concat [ accum.include_library_declarations; v ] }
          | Include_ci v                   -> {
                                                accum with
                                                include_ci_declarations =
                                                  List.concat [ accum.include_ci_declarations; v ];
                                              }
          | Cond_expand v                  -> { accum with cond_expands = List.concat [ accum.cond_expands; v ] })
        { Library_declaration.empty with name } declarations
    in
    L.pure declaration
  in
  Result.map fst @@ p v
