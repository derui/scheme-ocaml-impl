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

module T = Type

module Key_set = Set.Make (struct
  type t = string

  let compare = Stdlib.compare
end)

module Key_map = Map.Make (struct
  type t = string

  let compare = Stdlib.compare
end)

let import ~env ~declaration ~runtime:(module R : Runtime.S) =
  let import_sets = declaration.Import_declaration.import_sets in

  let rec import' = function
    | Import_set.Library_name name          -> (
        let library = R.get_library name in
        match library with
        | None         -> T.raise_error "Do not found library"
        | Some library -> Library.as_environment library |> Result.ok)
    | Import_set.Only (set, symbols)        ->
        let open Lib.Result.Let_syntax in
        let* env' = import' set in
        let new_env = Environment.make [] in
        List.iter
          (fun symbol ->
            Environment.get env' ~key:symbol |> Option.iter (fun v -> Environment.set new_env ~key:symbol ~v |> ignore))
          symbols;
        Ok new_env
    | Import_set.Except (set, symbols)      ->
        let open Lib.Result.Let_syntax in
        let* env' = import' set in
        let new_env = Environment.make [] in
        let except_keys = Key_set.of_list symbols and new_env_keys = Key_set.of_list @@ Environment.keys env' in
        let target_keys = Key_set.diff new_env_keys except_keys in
        Key_set.iter
          (fun symbol ->
            Environment.get env' ~key:symbol |> Option.iter (fun v -> Environment.set new_env ~key:symbol ~v |> ignore))
          target_keys;
        Ok new_env
    | Import_set.Prefix (set, prefix)       ->
        let open Lib.Result.Let_syntax in
        let* env' = import' set in
        let new_env = Environment.make [] in
        Environment.keys env'
        |> List.iter (fun symbol ->
               let new_key = prefix ^ symbol in

               Environment.get env' ~key:symbol
               |> Option.iter (fun v -> Environment.set new_env ~key:new_key ~v |> ignore));
        Ok new_env
    | Import_set.Rename (set, rename_pairs) ->
        let open Lib.Result.Let_syntax in
        let* env' = import' set in

        let new_env = Environment.make [] in
        let rename_map =
          List.map (fun v -> (v.Import_set.from_name, v.to_name)) rename_pairs |> List.to_seq |> Key_map.of_seq
        in
        Environment.keys env'
        |> List.iter (fun symbol ->
               let key = Key_map.find_opt symbol rename_map |> Option.value ~default:symbol in

               Environment.get env' ~key:symbol |> Option.iter (fun v -> Environment.set new_env ~key ~v |> ignore));
        Ok new_env
  in

  List.fold_left
    (fun accum import_set ->
      match accum with
      | Error _ -> accum
      | Ok env  ->
          let open Lib.Result.Let_syntax in
          let* env' = import' import_set in
          Environment.merge ~base:env ~other:env' |> Result.ok)
    (Ok env) import_sets
