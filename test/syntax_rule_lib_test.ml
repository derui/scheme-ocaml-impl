module S = Ocaml_scheme.Syntax_rule_lib
module T = Ocaml_scheme.Type
module Pr = Ocaml_scheme.Printer

let exp_pp = Alcotest.testable Pr.pp ( = )

let parser_pp p = Alcotest.(result (pair p @@ exp_pp) string)

let list_to_scheme_list v = List.rev v |> List.fold_left (fun accum v -> T.Cons (v, accum)) T.Empty_list

let list_parser_tests =
  [
    Alcotest.test_case "List Parser: parser a expression from list" `Quick (fun () ->
        let list = [ T.Symbol "foo" ] |> list_to_scheme_list in
        let actual = S.List_parser.expression list in
        let expected = Ok (T.Symbol "foo", T.Empty_list) in
        Alcotest.(check @@ parser_pp exp_pp) "simple" actual expected);
    Alcotest.test_case "List Parser: many combinator get zero or more repeated list" `Quick (fun () ->
        let p =
          S.List_parser.Infix.(
            (function T.Symbol s -> String.uppercase_ascii s | _ -> "other") <$> S.List_parser.expression)
        in
        let list = [ T.Symbol "foo"; T.Symbol "bar" ] |> list_to_scheme_list in
        let actual = S.List_parser.many p list in
        let expected = Ok ([ "FOO"; "BAR" ], T.Empty_list) in
        Alcotest.(check @@ parser_pp @@ list string) "simple" actual expected);
    Alcotest.test_case "List Parser: many1 combinator get  one or more repeated list" `Quick (fun () ->
        let p =
          S.List_parser.Infix.(
            (function T.Symbol s -> String.uppercase_ascii s | _ -> "other") <$> S.List_parser.expression)
        in
        let list = [ T.Symbol "foo"; T.Symbol "bar" ] |> list_to_scheme_list in
        let actual = S.List_parser.many p list in
        let expected = Ok ([ "FOO"; "BAR" ], T.Empty_list) in
        Alcotest.(check @@ parser_pp @@ list string) "simple" actual expected);
    Alcotest.test_case "List Parser: chainl1 chains operator between parsers" `Quick (fun () ->
        let p =
          S.List_parser.Infix.(
            (function T.Symbol s -> String.uppercase_ascii s | _ -> "other") <$> S.List_parser.expression)
        in
        let op = S.List_parser.pure @@ fun a b -> a ^ "___" ^ b in
        let list = [ T.Symbol "foo"; T.Symbol "bar" ] |> list_to_scheme_list in
        let actual = S.List_parser.(chainl1 p op) list in
        let expected = Ok ("FOO___BAR", T.Empty_list) in
        Alcotest.(check @@ parser_pp string) "simple" actual expected);
  ]

let tests = list_parser_tests
