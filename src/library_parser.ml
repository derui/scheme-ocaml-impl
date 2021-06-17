module L = List_parser
module T = Type

module Export_spec = struct
  type t =
    | Ident  of string
    | Rename of string * string

  let show = function Ident v -> v | Rename (v1, v2) -> Printf.sprintf "(rename %s %s)" v1 v2
end

module Library_declaration = struct
  type t = {
    export_declaration : Export_spec.t list;
    import_declaration : Import.Import_declaration.t list;
    begin_declaration : T.data list;
    include_declaration : string list;
    include_ci_declaration : string list;
  }

  let show t =
    let export =
      Printf.sprintf "(export %s)" (t.export_declaration |> List.map Export_spec.show |> String.concat " ")
    in
    let import = t.import_declaration |> List.map Import.Import_declaration.show |> String.concat " " in
    let defs = t.begin_declaration |> List.map Printer.print |> String.concat " " in
    let includes = t.include_declaration |> String.concat " " in
    let include_cis = t.include_ci_declaration |> String.concat " " in
    Printf.sprintf "(%s %s (begin %s) (include %s) (include-ci %s))" export import defs includes include_cis

  let pp fmt v = Format.fprintf fmt "%s" @@ show v

  let empty =
    {
      export_declaration = [];
      import_declaration = [];
      begin_declaration = [];
      include_declaration = [];
      include_ci_declaration = [];
    }
end

type declaration =
  | Export     of Export_spec.t list
  | Import     of Import.Import_declaration.t
  | Begin      of T.data list
  | Include    of string list
  | Include_ci of string list

let symbol = L.satisfy (function T.Symbol _ -> true | _ -> false)

let string = L.satisfy (function T.Scheme_string _ -> true | _ -> false)

let pair = L.satisfy (function T.Cons _ -> true | _ -> false)

let identifiers =
  let open L.Infix in
  let to_list v =
    List.fold_left (fun accum v -> match v with T.Symbol ident -> ident :: accum | _ -> accum) [] v |> List.rev
  in
  to_list <$> L.many symbol

let identifier =
  let open L.Let_syntax in
  let* v = symbol in
  match v with T.Symbol v -> L.pure v | _ -> L.zero

let export_spec =
  let open L.Let_syntax in
  let ident =
    let* v = identifier in
    L.pure (Export_spec.Ident v)
  and rename =
    print_endline "in rename";
    let* pair = pair in
    let p' =
      let* _ = L.satisfy (function T.Symbol "rename" -> true | _ -> false) in
      let* from_mame = identifier in
      let* to_name = identifier in
      L.pure (Export_spec.Rename (from_mame, to_name))
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
    let* import_declaration = Import.Parser.import_declaration in
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

let parse v =
  let open L.Let_syntax in
  let p =
    let* _ = pair in
    let* declarations =
      L.(
        many
        @@ (export_declaration <|> import_declaration <|> begin_declaration <|> include_declaration
          <|> include_ci_declaration))
    in

    let declaration =
      List.fold_left
        (fun (accum : Library_declaration.t) v ->
          match v with
          | Begin v      -> { accum with begin_declaration = List.concat [ accum.begin_declaration; v ] }
          | Export v     -> { accum with export_declaration = List.concat [ accum.export_declaration; v ] }
          | Import v     -> { accum with import_declaration = List.concat [ accum.import_declaration; [ v ] ] }
          | Include v    -> { accum with include_declaration = List.concat [ accum.include_declaration; v ] }
          | Include_ci v -> { accum with include_ci_declaration = List.concat [ accum.include_ci_declaration; v ] })
        Library_declaration.empty declarations
    in
    L.pure declaration
  in
  Result.map fst @@ p v
