module LD = Library_declaration
module L = Library

type library_path = string

module type Library_conf = sig
  val library_paths : library_path list
  (** [library_paths] define list of path for library loading *)
end

module Make (Library_conf : Library_conf) (Runtime : Runtime.S) : Library_producer.S = struct
  let find_library name =
    let library = String.concat Filename.dir_sep name in
    Library_conf.library_paths
    |> List.find_opt (fun path ->
           let library_path = Filename.concat path library ^ ".scm" in
           Sys.file_exists library_path)

  let exists name = find_library name |> Option.map (fun _ -> true) |> Option.value ~default:false

  let define_from_env library env =
    let keys = Environment.keys env in
    List.fold_left
      (fun accum key -> L.define ~symbol:key ~data:(Environment.get ~key env |> Option.get) accum)
      library keys

  let merge_export library library_declaration =
    List.fold_left
      (fun accum export ->
        match export with
        | LD.Export_spec.Ident v                  -> L.export ~symbol:v accum
        | LD.Export_spec.Rename (origin, renamed) -> L.export ~symbol:origin ~renamed accum)
      library library_declaration.LD.export_declarations

  let produce name =
    let open Lib.Result.Let_syntax in
    let library =
      let* library_file =
        match find_library name with None -> Type.raise_error "Not found library" | Some v -> Ok v
      in
      let* library_exp =
        Lib.File.with_channel library_file ~f:(fun chan ->
            let programs = Lexing.from_channel chan |> Parser.program Lexer.token in
            match programs with
            | []           -> Type.raise_error
                              @@ Printf.sprintf "can not find library %s" (String.concat Filename.dir_sep name)
            | program :: _ -> Ok program)
      in
      let* library_declaration = Library_parser.parse library_exp in
      (* TODO: expand in parser. *)
      let _ =
        library_declaration.LD.cond_expands |> List.filter_map (Cond_expand.eval ~runtime:(module Runtime)) |> List.rev
      in
      let library = List.map Printer.print library_declaration.LD.name |> Library.make in
      let import_declarations = library_declaration.LD.import_declarations in
      let env = Environment.make [] in
      let* imported_env =
        List.fold_left
          (fun env decl ->
            match env with Error _ -> env | Ok env -> Import.import ~env ~runtime:(module Runtime) ~declaration:decl)
          (Ok env) import_declarations
      in
      let () =
        List.iter
          (fun decl -> Eval.eval ~runtime:(module Runtime) ~env:imported_env decl |> ignore)
          library_declaration.begin_declarations
      in
      let library = merge_export library library_declaration in
      let library = define_from_env library imported_env in
      Ok library
    in
    Result.to_option library
end
