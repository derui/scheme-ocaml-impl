module T = Type

module Export_spec = struct
  type t =
    | Ident  of string
    | Rename of string * string

  let show = function Ident v -> v | Rename (v1, v2) -> Printf.sprintf "(rename %s %s)" v1 v2
end

type t = {
  name : T.data list;
  export_declarations : Export_spec.t list;
  import_declarations : Import.Import_declaration.t list;
  begin_declarations : T.data list;
  include_declarations : string list;
  include_ci_declarations : string list;
  include_library_declarations : string list;
  cond_expands : Cond_expand.t list;
}

let show t =
  let export = Printf.sprintf "(export %s)" (t.export_declarations |> List.map Export_spec.show |> String.concat " ") in
  let import = t.import_declarations |> List.map Import.Import_declaration.show |> String.concat " " in
  let defs = t.begin_declarations |> List.map Printer.print |> String.concat " " in
  let includes = t.include_declarations |> String.concat " " in
  let include_cis = t.include_ci_declarations |> String.concat " " in
  let include_library_declarations = t.include_library_declarations |> String.concat " " in
  let cond_expands = List.map Cond_expand.show t.cond_expands |> String.concat " " in
  Printf.sprintf "(%s %s (begin %s) (include %s) (include-ci %s) (include-library-declarations %s) %s)" export import
    defs includes include_cis include_library_declarations cond_expands

let pp fmt v = Format.fprintf fmt "%s" @@ show v

let empty =
  {
    name = [];
    export_declarations = [];
    import_declarations = [];
    begin_declarations = [];
    include_declarations = [];
    include_ci_declarations = [];
    include_library_declarations = [];
    cond_expands = [];
  }
