module P = Ocaml_scheme.Parser
module Lexer = Ocaml_scheme.Lexer
module T = Ocaml_scheme.Type
module D = Ocaml_scheme.Data_type
module L = Ocaml_scheme.Library
module Pr = Ocaml_scheme.Printer
module E = Ocaml_scheme.Environment

let exp = Alcotest.testable Pr.pp ( = )

let library = Alcotest.testable L.pp ( = )

let error = Alcotest.of_pp T.Scheme_error.pp

let parse str = Lexing.from_string str |> P.program Lexer.token |> List.hd

let to_list = Ocaml_scheme.Internal_lib.list_to_scheme_list

let tests =
  [
    Alcotest.test_case "get name of the library" `Quick (fun () ->
        let actual = L.make [ "base"; "name" ] |> L.name in
        let expected = [ "base"; "name" ] in
        Alcotest.(check @@ list string) "result" expected actual);
    Alcotest.test_case "make empty library" `Quick (fun () ->
        let actual = L.make [ "base" ] |> L.exports in
        let expected = [] in
        Alcotest.(check @@ list string) "result" expected actual);
    Alcotest.test_case "export a definition" `Quick (fun () ->
        let actual = L.make [ "base" ] |> L.export ~symbol:"foo" |> L.exports in
        let expected = [ "foo" ] in
        Alcotest.(check @@ list string) "result" expected actual);
    Alcotest.test_case "define a data in library" `Quick (fun () ->
        let data = parse "1" in
        let actual = L.make [ "base" ] |> L.define ~symbol:"foo" ~data |> L.export ~symbol:"foo" |> L.as_environment in
        let expected = Some (T.Number "1") in
        Alcotest.(check @@ option exp) "result" expected (E.get ~key:"foo" actual));
    Alcotest.test_case "can rename export" `Quick (fun () ->
        let data = parse "1" in
        let actual =
          L.make [ "base" ] |> L.define ~symbol:"foo" ~data |> L.export ~symbol:"foo" ~renamed:"bar" |> L.as_environment
        in
        let expected = Some (T.Number "1") in
        Alcotest.(check @@ option exp) "result" expected (E.get ~key:"bar" actual));
  ]
