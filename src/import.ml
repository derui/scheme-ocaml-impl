(** Import contains three inner module.

    - Parser
    - It is the parser for import expression
    - Importer
    - It has function to import library into specified environment *)

module Import_set = struct
  type rename_pair = {
    from_name : string;
    to_name : string;
  }

  type t =
    | Only         of (t * string list)
    | Except       of (t * string list)
    | Prefix       of (t * string)
    | Rename       of (t * rename_pair list)
    | Library_name of string list

  let rec show = function
    | Only (v, list)     -> Printf.sprintf "(only %s %s)" (show v) (String.concat " " list)
    | Except (v, list)   -> Printf.sprintf "(except %s %s)" (show v) (String.concat " " list)
    | Prefix (v, prefix) -> Printf.sprintf "(prefix %s %s)" (show v) prefix
    | Rename (v, list)   ->
        let show_rename { from_name; to_name } = Printf.sprintf "(%s %s)" from_name to_name in
        Printf.sprintf "(rename %s %s)" (show v) (List.map show_rename list |> String.concat " ")
    | Library_name name  -> Printf.sprintf "(%s)" (String.concat " " name)
end

module Import_declaration = struct
  type t = { import_sets : Import_set.t list }

  let show v = Printf.sprintf "(import %s)" @@ (List.map Import_set.show v.import_sets |> String.concat " ")

  let pp fmt v = Format.fprintf fmt "%s" @@ show v
end

module Parser = struct
  module L = List_parser
  module T = Type

  let symbol = L.satisfy (function T.Symbol _ -> true | _ -> false)

  let pair = L.satisfy (function T.Cons _ -> true | _ -> false)

  let identifiers =
    let open L.Infix in
    let to_list v =
      List.fold_left (fun accum v -> match v with T.Symbol ident -> ident :: accum | _ -> accum) [] v |> List.rev
    in
    to_list <$> L.many1 symbol

  let rec import_set v =
    let open L.Infix in
    let open L.Let_syntax in
    let only v =
      let p =
        let* pair = pair in
        let p' =
          let* _ = L.satisfy (function T.Symbol "only" -> true | _ -> false) in
          let* set = import_set in
          let* identifiers = identifiers in
          L.pure (Import_set.Only (set, identifiers))
        in
        fun _ -> p' pair
      in
      p v
    and except v =
      let open L.Let_syntax in
      let p =
        let* pair = pair in
        let p' =
          let* _ = L.satisfy (function T.Symbol "except" -> true | _ -> false) in
          let* set = import_set in
          let* identifiers = identifiers in
          L.pure (Import_set.Except (set, identifiers))
        in
        fun _ -> p' pair
      in
      p v
    and prefix v =
      let open L.Let_syntax in
      let p =
        let* pair = pair in
        let p' =
          let* _ = L.satisfy (function T.Symbol "prefix" -> true | _ -> false) in
          let* set = import_set in
          let* identifier = (function T.Symbol v -> v | _ -> failwith "Invalid path") <$> symbol in
          L.pure (Import_set.Prefix (set, identifier))
        in
        fun _ -> p' pair
      in
      p v
    and rename v =
      let open L.Let_syntax in
      let p =
        let* element = pair in

        let p' =
          let* _ = L.satisfy (function T.Symbol "rename" -> true | _ -> false) in
          let* set = import_set in
          let* renames =
            List.map (function
              | T.Cons { car = T.Symbol from; cdr = T.Cons { car = T.Symbol to_name; cdr = T.Empty_list } } ->
                  { Import_set.from_name = from; to_name }
              | _ -> failwith "Invalid path")
            <$> L.many1 pair
          in
          L.pure (Import_set.Rename (set, renames))
        in
        fun _ -> p' element
      in
      p v
    and library_name v =
      let open L.Let_syntax in
      let open L.Infix in
      let p =
        let* library_name, _ = Internal_lib.scheme_list_to_list <$> pair in
        L.pure (Import_set.Library_name (List.filter_map (function T.Symbol v -> Some v | _ -> None) library_name))
      in
      p v
    in
    L.(only <|> except <|> prefix <|> rename <|> library_name) v

  let import_declaration =
    let open L.Let_syntax in
    let* _ = L.satisfy (function T.Symbol "import" -> true | _ -> false) in
    let* import_sets = L.many import_set in
    L.pure { Import_declaration.import_sets }

  let parse v = Result.map fst @@ import_declaration v
end
