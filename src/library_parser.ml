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
    import_declaration : Import.Import_declaration.t option;
    begin_declaration : T.data list;
  }

  let show t =
    let export =
      Printf.sprintf "(export %s)" (t.export_declaration |> List.map Export_spec.show |> String.concat " ")
    in
    let import = t.import_declaration |> Option.map Import.Import_declaration.show |> Option.value ~default:"" in
    let defs = t.begin_declaration |> List.map Printer.print |> String.concat " " in
    Printf.sprintf "(%s %s (begin %s))" export import defs

  let pp fmt v = Format.fprintf fmt "%s" @@ show v

  let empty = { export_declaration = []; import_declaration = None; begin_declaration = [] }
end

type declaration =
  | Export of Export_spec.t list
  | Import of Import.Import_declaration.t
  | Begin  of T.data list

let symbol = L.satisfy (function T.Symbol _ -> true | _ -> false)

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
    fun _ -> p' pair
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
  fun _ -> p pair

let export_declaration =
  let open L.Let_syntax in
  let* pair = pair in
  let p =
    let* _ = L.satisfy (function T.Symbol "export" -> true | _ -> false) in
    let* specs = L.many export_spec in
    L.pure (Export specs)
  in
  fun _ -> p pair

let import_declaration =
  let open L.Let_syntax in
  let* pair = pair in
  let p =
    let* import_declaration = Import.Parser.import_declaration in
    L.pure (Import import_declaration)
  in
  fun _ -> p pair

let parse v =
  let open L.Let_syntax in
  let p =
    let* _ = pair in
    let* declarations = L.(export_declaration <|> import_declaration <|> begin_declaration) |> L.many in

    let declaration =
      List.fold_left
        (fun accum v ->
          match v with
          | Begin v  -> { accum with Library_declaration.begin_declaration = v }
          | Export v -> { accum with export_declaration = v }
          | Import v -> { accum with import_declaration = Some v })
        Library_declaration.empty declarations
    in
    L.pure declaration
  in
  Result.map fst @@ p v
