module LD = Library_declaration

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
      let expanded =
        library_declaration.LD.cond_expands |> List.filter_map (Cond_expand.eval ~runtime:(module Runtime))
      in
      let library = List.map Printer.print library_declaration.LD.name |> Library.make in
      let import_declaration = library_declaration.LD.import_declarations in
      failwith ""
    in
    None

  let import_to_env env import_declaration = failwith ""
end
