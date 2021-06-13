module P = Ocaml_scheme.Library_parser
module I = Ocaml_scheme.Import
module T = Ocaml_scheme.Type
module Pr = Ocaml_scheme.Printer

let exp_pp = Alcotest.testable Pr.pp ( = )

let error_t = Alcotest.testable T.Scheme_error.pp ( = )

let library_t = Alcotest.testable P.Library_declaration.pp ( = )

let parse v = Lexing.from_string v |> Ocaml_scheme.Parser.program Ocaml_scheme.Lexer.token |> List.hd

let list_to_scheme_list v = List.rev v |> List.fold_left (fun accum v -> T.cons v accum) T.Empty_list

let tests =
  [
    Alcotest.test_case "Library parser: parse empty declaration" `Quick (fun () ->
        let v = parse "((a b))" in
        let actual = P.parse v in
        let expected = Ok P.Library_declaration.empty in
        Alcotest.(check @@ result library_t error_t) "simple" expected actual);
    Alcotest.test_case "Library parser: parse export declaration" `Quick (fun () ->
        let v = parse "((a b) (export a b (rename c d)))" in
        let actual = P.parse v in
        let expected =
          Ok
            {
              P.Library_declaration.empty with
              P.Library_declaration.export_declaration = [ P.Export_spec.Ident "a"; Ident "b"; Rename ("c", "d") ];
            }
        in
        Alcotest.(check @@ result library_t error_t) "simple" expected actual);
    Alcotest.test_case "Library parser: parse begin declaration" `Quick (fun () ->
        let v = parse "((a b) (begin a b 1))" in
        let actual = P.parse v in
        let expected =
          Ok
            {
              P.Library_declaration.empty with
              P.Library_declaration.begin_declaration = [ T.Symbol "a"; T.Symbol "b"; T.Number "1" ];
            }
        in
        Alcotest.(check @@ result library_t error_t) "simple" expected actual);
    Alcotest.test_case "Library parser: parse import declaration" `Quick (fun () ->
        let v = parse "((a b) (import (a b)))" in
        let actual = P.parse v in
        let expected =
          Ok
            {
              P.Library_declaration.empty with
              P.Library_declaration.import_declaration =
                Some { I.Import_declaration.import_sets = [ I.Import_set.Library_name [ "a"; "b" ] ] };
            }
        in
        Alcotest.(check @@ result library_t error_t) "simple" expected actual);
  ]
