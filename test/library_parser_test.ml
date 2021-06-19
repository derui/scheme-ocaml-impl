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
        let expected = Ok { P.Library_declaration.empty with name = [ T.Symbol "a"; T.Symbol "b" ] } in
        Alcotest.(check @@ result library_t error_t) "simple" expected actual);
    Alcotest.test_case "Library parser: parse export declaration" `Quick (fun () ->
        let v = parse "((a b) (export a b (rename c d)))" in
        let actual = P.parse v in
        let expected =
          Ok
            {
              P.Library_declaration.empty with
              export_declaration = [ P.Export_spec.Ident "a"; Ident "b"; Rename ("c", "d") ];
              name = [ T.Symbol "a"; T.Symbol "b" ];
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
              begin_declaration = [ T.Symbol "a"; T.Symbol "b"; T.Number "1" ];
              name = [ T.Symbol "a"; T.Symbol "b" ];
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
              import_declaration = [ { I.Import_declaration.import_sets = [ I.Import_set.Library_name [ "a"; "b" ] ] } ];
              name = [ T.Symbol "a"; T.Symbol "b" ];
            }
        in
        Alcotest.(check @@ result library_t error_t) "simple" expected actual);
    Alcotest.test_case "Library parser: parse some declaration" `Quick (fun () ->
        let v = parse "((a b) (import (a b)) (begin a b 1))" in
        let actual = P.parse v in
        let expected =
          Ok
            {
              P.Library_declaration.empty with
              import_declaration = [ { I.Import_declaration.import_sets = [ I.Import_set.Library_name [ "a"; "b" ] ] } ];
              begin_declaration = [ T.Symbol "a"; T.Symbol "b"; T.Number "1" ];
              name = [ T.Symbol "a"; T.Symbol "b" ];
            }
        in
        Alcotest.(check @@ result library_t error_t) "simple" expected actual);
    Alcotest.test_case "Library parser: parse include declaration" `Quick (fun () ->
        let v = parse {|( (a b) (include "foo" "bar") )|} in
        let actual = P.parse v in
        let expected =
          Ok
            {
              P.Library_declaration.empty with
              include_declaration = [ "foo"; "bar" ];
              name = [ T.Symbol "a"; T.Symbol "b" ];
            }
        in
        Alcotest.(check @@ result library_t error_t) "simple" expected actual);
    Alcotest.test_case "Library parser: parse include declaration multiple times" `Quick (fun () ->
        let v = parse {| ((a b) (include "foo" "bar") (include "foobar.scm")) |} in
        let actual = P.parse v in
        let expected =
          Ok
            {
              P.Library_declaration.empty with
              include_declaration = [ "foo"; "bar"; "foobar.scm" ];
              name = [ T.Symbol "a"; T.Symbol "b" ];
            }
        in
        Alcotest.(check @@ result library_t error_t) "simple" expected actual);
    Alcotest.test_case "Library parser: parse include-ci declaration" `Quick (fun () ->
        let v = parse {|( (a b) (include-ci "foo" "bar") )|} in
        let actual = P.parse v in
        let expected =
          Ok
            {
              P.Library_declaration.empty with
              include_ci_declaration = [ "foo"; "bar" ];
              name = [ T.Symbol "a"; T.Symbol "b" ];
            }
        in
        Alcotest.(check @@ result library_t error_t) "simple" expected actual);
    Alcotest.test_case "Library parser: parse include-ci declaration multiple times" `Quick (fun () ->
        let v = parse {|( (a b) (include-ci "foo" "bar") (include-ci "baz"))|} in
        let actual = P.parse v in
        let expected =
          Ok
            {
              P.Library_declaration.empty with
              include_ci_declaration = [ "foo"; "bar"; "baz" ];
              name = [ T.Symbol "a"; T.Symbol "b" ];
            }
        in
        Alcotest.(check @@ result library_t error_t) "simple" expected actual);
    Alcotest.test_case "Library parser: parse include-library-declarations declaration" `Quick (fun () ->
        let v = parse {|( (a b) (include-library-declarations "foo" "bar") )|} in
        let actual = P.parse v in
        let expected =
          Ok
            {
              P.Library_declaration.empty with
              include_library_declarations = [ "foo"; "bar" ];
              name = [ T.Symbol "a"; T.Symbol "b" ];
            }
        in
        Alcotest.(check @@ result library_t error_t) "simple" expected actual);
    Alcotest.test_case "Library parser: parse include-library-declarations declaration multiple times" `Quick (fun () ->
        let v = parse {|( (a b) (include-library-declarations "foo" "bar") (include-library-declarations "baz"))|} in
        let actual = P.parse v in
        let expected =
          Ok
            {
              P.Library_declaration.empty with
              include_library_declarations = [ "foo"; "bar"; "baz" ];
              name = [ T.Symbol "a"; T.Symbol "b" ];
            }
        in
        Alcotest.(check @@ result library_t error_t) "simple" expected actual);
  ]
