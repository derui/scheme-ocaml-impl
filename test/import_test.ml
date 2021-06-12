module P = Ocaml_scheme.Import.Parser
module I = Ocaml_scheme.Import
module T = Ocaml_scheme.Type
module Pr = Ocaml_scheme.Printer

let exp_pp = Alcotest.testable Pr.pp ( = )

let error_t = Alcotest.testable T.Scheme_error.pp ( = )

let import_set_t = Alcotest.testable I.Import_declaration.pp ( = )

let parse v = Lexing.from_string v |> Ocaml_scheme.Parser.program Ocaml_scheme.Lexer.token |> List.hd

let list_to_scheme_list v = List.rev v |> List.fold_left (fun accum v -> T.cons v accum) T.Empty_list

let parser_tests =
  [
    Alcotest.test_case "Import parser: parse empty declaration" `Quick (fun () ->
        let v = parse "(import)" in
        let actual = P.parse v in
        let expected = Ok { I.Import_declaration.import_sets = [] } in
        Alcotest.(check @@ result import_set_t error_t) "simple" expected actual);
    Alcotest.test_case "Import parser: parse 'only' declaration" `Quick (fun () ->
        let v = parse "(import (only (foo) a b c))" in
        let actual = P.parse v in
        let expected = Ok { I.Import_declaration.import_sets = [ Only (Library_name [ "foo" ], [ "a"; "b"; "c" ]) ] } in
        Alcotest.(check @@ result import_set_t error_t) "simple" expected actual);
    Alcotest.test_case "Import parser: parse 'except' declaration" `Quick (fun () ->
        let v = parse "(import (except (foo bar) a b c))" in
        let actual = P.parse v in
        let expected =
          Ok { I.Import_declaration.import_sets = [ Except (Library_name [ "foo"; "bar" ], [ "a"; "b"; "c" ]) ] }
        in
        Alcotest.(check @@ result import_set_t error_t) "simple" expected actual);
    Alcotest.test_case "Import parser: parse 'prefix' declaration" `Quick (fun () ->
        let v = parse "(import (prefix (foo bar) pre-))" in
        let actual = P.parse v in
        let expected = Ok { I.Import_declaration.import_sets = [ Prefix (Library_name [ "foo"; "bar" ], "pre-") ] } in
        Alcotest.(check @@ result import_set_t error_t) "simple" expected actual);
    Alcotest.test_case "Import parser: parse 'rename' declaration" `Quick (fun () ->
        let v = parse "(import (rename (foo bar) (a b) (c d)))" in
        let actual = P.parse v in
        let expected =
          Ok
            {
              I.Import_declaration.import_sets =
                [
                  Rename
                    ( Library_name [ "foo"; "bar" ],
                      [ { from_name = "a"; to_name = "b" }; { from_name = "c"; to_name = "d" } ] );
                ];
            }
        in
        Alcotest.(check @@ result import_set_t error_t) "simple" expected actual);
    Alcotest.test_case "Import parser: parse simple library name declaration" `Quick (fun () ->
        let v = parse "(import (only except library name))" in
        let actual = P.parse v in
        let expected =
          Ok { I.Import_declaration.import_sets = [ Library_name [ "only"; "except"; "library"; "name" ] ] }
        in
        Alcotest.(check @@ result import_set_t error_t) "simple" expected actual);
    Alcotest.test_case "Import parser: parse multiple declarations" `Quick (fun () ->
        let v = parse "(import (library name) (only (foo) test))" in
        let actual = P.parse v in
        let expected =
          Ok
            {
              I.Import_declaration.import_sets =
                [ Library_name [ "library"; "name" ]; Only (Library_name [ "foo" ], [ "test" ]) ];
            }
        in
        Alcotest.(check @@ result import_set_t error_t) "simple" expected actual);
  ]

let tests = parser_tests
