module L = Ocaml_scheme.List_parser
module T = Ocaml_scheme.Type
module Pr = Ocaml_scheme.Printer

let exp_pp = Alcotest.testable Pr.pp ( = )

let element_p = Alcotest.(of_pp Pr.pp)

let error_t = Alcotest.testable T.Scheme_error.pp ( = )

let parser_pp p = Alcotest.(result (pair p exp_pp) error_t)

let parse v = Lexing.from_string v |> Ocaml_scheme.Parser.program Ocaml_scheme.Lexer.token |> List.hd

let list_to_scheme_list v = List.rev v |> List.fold_left (fun accum v -> T.cons v accum) T.Empty_list

let tests =
  [
    Alcotest.test_case "parser a expression from list" `Quick (fun () ->
        let list = [ T.Symbol "foo" ] |> list_to_scheme_list in
        let actual = L.element list in
        let expected = Ok (T.Symbol "foo", T.Empty_list) in
        Alcotest.(check @@ parser_pp @@ element_p) "simple" expected actual);
    Alcotest.test_case "parser a cdr of list" `Quick (fun () ->
        let list = parse "(a . b)" in
        let actual = L.element list in
        let expected = Ok (T.Symbol "a", T.Symbol "b") in
        Alcotest.(check @@ parser_pp element_p) "simple" expected actual);
    Alcotest.test_case "many combinator get zero or more repeated list" `Quick (fun () ->
        let p = L.Infix.((function T.Symbol s -> String.uppercase_ascii s | _ -> "other") <$> L.element) in
        let list = parse "(foo bar)" in
        let actual = L.many p list in
        let expected = Ok ([ "FOO"; "BAR" ], T.Empty_list) in
        Alcotest.(check @@ parser_pp @@ list string) "simple" expected actual);
    Alcotest.test_case "many1 combinator get  one or more repeated list" `Quick (fun () ->
        let p = L.Infix.((function T.Symbol s -> String.uppercase_ascii s | _ -> "other") <$> L.element) in
        let list = parse "(foo bar)" in
        let actual = L.many p list in
        let expected = Ok ([ "FOO"; "BAR" ], T.Empty_list) in
        Alcotest.(check @@ parser_pp @@ list string) "simple" expected actual);
    Alcotest.test_case "chainl1 chains operator between parsers" `Quick (fun () ->
        let p = L.Infix.((function T.Symbol s -> String.uppercase_ascii s | _ -> "other") <$> L.element) in
        let op = L.pure @@ fun a b -> a ^ "___" ^ b in
        let list = [ T.Symbol "foo"; T.Symbol "bar" ] |> list_to_scheme_list in
        let actual = L.(chainl1 p op) list in
        let expected = Ok ("FOO___BAR", T.Empty_list) in
        Alcotest.(check @@ parser_pp string) "simple" expected actual);
    Alcotest.test_case "expression allow to parse dotted-list" `Quick (fun () ->
        let open L.Let_syntax in
        let list = parse "(a . b)" in
        let p =
          let* _ = L.element in
          L.cdr
        in
        let actual = p list in
        let expected = Ok (T.Symbol "b", T.Empty_list) in
        Alcotest.(check @@ parser_pp @@ element_p) "simple" expected actual);
  ]
