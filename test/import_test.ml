module L = Ocaml_scheme.Library
module I = Ocaml_scheme.Import
module T = Ocaml_scheme.Type
module R = Ocaml_scheme.Runtime
module Env = Ocaml_scheme.Environment
module Pr = Ocaml_scheme.Printer

let exp_pp = Alcotest.testable Pr.pp ( = )

let error_t = Alcotest.testable T.Scheme_error.pp ( = )

let import_set_t = Alcotest.testable I.Import_declaration.pp ( = )

let parse v = Lexing.from_string v |> Ocaml_scheme.Parser.program Ocaml_scheme.Lexer.token |> List.hd

let list_to_scheme_list v = List.rev v |> List.fold_left (fun accum v -> T.cons v accum) T.Empty_list

let tests =
  let library =
    L.make [ "name" ]
    |> L.define ~symbol:"bool" ~data:T.True |> L.export ~symbol:"bool"
    |> L.define ~symbol:"number" ~data:(T.Number "15")
    |> L.export ~symbol:"number"
  in
  let module Runtime : R.S = struct
    let get_library _ = Some library

    let define_library _ = failwith "not implemented"

    let is_requirement_filled _ = true
  end in
  [
    Alcotest.test_case "import all symbols from library" `Quick (fun () ->
        let import_decl = I.Import_declaration.{ import_sets = [ I.Import_set.Library_name [ "name" ] ] } in
        let env = I.import ~env:(Env.make []) ~runtime:(module Runtime) ~declaration:import_decl |> Result.get_ok in
        let actual = Env.get env ~key:"bool" in
        let expected = Some T.True in
        Alcotest.(check @@ option exp_pp) "bool" expected actual);
    Alcotest.test_case "do not import symbol not exported from library" `Quick (fun () ->
        let import_decl = I.Import_declaration.{ import_sets = [ I.Import_set.Library_name [ "name" ] ] } in
        let env = I.import ~env:(Env.make []) ~runtime:(module Runtime) ~declaration:import_decl |> Result.get_ok in
        let actual = Env.get env ~key:"not-found" in
        let expected = None in
        Alcotest.(check @@ option exp_pp) "not found" expected actual);
    Alcotest.test_case "renamed import " `Quick (fun () ->
        let import_decl =
          I.Import_declaration.
            {
              import_sets =
                [
                  I.Import_set.(Rename (Library_name [ "name" ], [ { from_name = "bool"; to_name = "renamed-bool" } ]));
                ];
            }
        in
        let env = I.import ~env:(Env.make []) ~runtime:(module Runtime) ~declaration:import_decl |> Result.get_ok in
        let actual = Env.get env ~key:"renamed-bool" in
        let expected = Some T.True in
        Alcotest.(check @@ option exp_pp) "not found" expected actual);
    Alcotest.test_case "import only specified symbols" `Quick (fun () ->
        let import_decl =
          I.Import_declaration.{ import_sets = [ I.Import_set.(Only (Library_name [ "name" ], [ "bool" ])) ] }
        in
        let env = I.import ~env:(Env.make []) ~runtime:(module Runtime) ~declaration:import_decl |> Result.get_ok in
        let actual = Env.get env ~key:"bool" in
        let expected = Some T.True in
        Alcotest.(check @@ option exp_pp) "not found" expected actual;
        let actual = Env.get env ~key:"number" in
        let expected = None in
        Alcotest.(check @@ option exp_pp) "not found" expected actual);
  ]
