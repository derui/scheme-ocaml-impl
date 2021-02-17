module S = Ocaml_scheme.Syntax_rule_lib
module T = Ocaml_scheme.Type
module Pr = Ocaml_scheme.Printer

let exp_pp = Alcotest.testable Pr.pp ( = )

let element_p =
  let element_show = function
    | `Car v -> Printf.sprintf "car(%s)" @@ Pr.print v
    | `Cdr v -> Printf.sprintf "cdr(%s)" @@ Pr.print v
  in
  let element_pp fmt v = Format.fprintf fmt "%s" @@ element_show v in
  Alcotest.(of_pp element_pp)

let parser_pp p = Alcotest.(result (pair p @@ option exp_pp) string)

let list_to_scheme_list v = List.rev v |> List.fold_left (fun accum v -> T.Cons (v, accum)) T.Empty_list

let list_parser_tests =
  [
    Alcotest.test_case "List Parser: parser a expression from list" `Quick (fun () ->
        let list = [ T.Symbol "foo" ] |> list_to_scheme_list in
        let actual = S.List_parser.element (Some list) in
        let expected = Ok (`Car (T.Symbol "foo"), Some T.Empty_list) in
        Alcotest.(check @@ parser_pp @@ element_p) "simple" expected actual);
    Alcotest.test_case "List Parser: parser a cdr of list" `Quick (fun () ->
        let list = T.Constructor.(cons (symbol "a") (symbol "b")) in
        let actual = S.List_parser.element (Some list) in
        let expected = Ok (`Car (T.Symbol "a"), Some (T.Symbol "b")) in
        Alcotest.(check @@ parser_pp element_p) "simple" expected actual);
    Alcotest.test_case "List Parser: many combinator get zero or more repeated list" `Quick (fun () ->
        let p =
          S.List_parser.Infix.(
            (function `Car (T.Symbol s) -> String.uppercase_ascii s | _ -> "other") <$> S.List_parser.element)
        in
        let list = [ T.Symbol "foo"; T.Symbol "bar" ] |> list_to_scheme_list in
        let actual = S.List_parser.many p (Some list) in
        let expected = Ok ([ "FOO"; "BAR" ], Some T.Empty_list) in
        Alcotest.(check @@ parser_pp @@ list string) "simple" expected actual);
    Alcotest.test_case "List Parser: many1 combinator get  one or more repeated list" `Quick (fun () ->
        let p =
          S.List_parser.Infix.(
            (function `Car (T.Symbol s) -> String.uppercase_ascii s | _ -> "other") <$> S.List_parser.element)
        in
        let list = [ T.Symbol "foo"; T.Symbol "bar" ] |> list_to_scheme_list in
        let actual = S.List_parser.many p (Some list) in
        let expected = Ok ([ "FOO"; "BAR" ], Some T.Empty_list) in
        Alcotest.(check @@ parser_pp @@ list string) "simple" expected actual);
    Alcotest.test_case "List Parser: chainl1 chains operator between parsers" `Quick (fun () ->
        let p =
          S.List_parser.Infix.(
            (function `Car (T.Symbol s) -> String.uppercase_ascii s | _ -> "other") <$> S.List_parser.element)
        in
        let op = S.List_parser.pure @@ fun a b -> a ^ "___" ^ b in
        let list = [ T.Symbol "foo"; T.Symbol "bar" ] |> list_to_scheme_list in
        let actual = S.List_parser.(chainl1 p op) (Some list) in
        let expected = Ok ("FOO___BAR", Some T.Empty_list) in
        Alcotest.(check @@ parser_pp string) "simple" expected actual);
    Alcotest.test_case "List Parser: expression allow to parse dotted-list" `Quick (fun () ->
        let open S.List_parser.Let_syntax in
        let list = T.Constructor.(cons (symbol "a") (symbol "b")) in
        let p =
          let* _ = S.List_parser.element in
          S.List_parser.element
        in
        let actual = p (Some list) in
        let expected = Ok (`Cdr (T.Symbol "b"), None) in
        Alcotest.(check @@ parser_pp @@ element_p) "simple" expected actual);
  ]

let pattern_parser_test =
  let pattern_p = Alcotest.of_pp S.Pattern.pp in
  let pattern_in_rule_pp = Alcotest.(list pattern_p) in
  let rule_pp = Alcotest.(result (pair pattern_in_rule_pp @@ option exp_pp) string) in
  let module P = S.Pattern in
  [
    Alcotest.test_case "Rule Parser: parse the simplest pattern" `Quick (fun () ->
        let list = [ T.Symbol "a" ] |> list_to_scheme_list in
        let actual = S.Rule_parser.pattern_in_rule (Some list) in
        let expected = Ok ([], Some T.Empty_list) in
        Alcotest.(check rule_pp) "simple" expected actual);
    Alcotest.test_case "Rule Parser: parse one symbol pattern" `Quick (fun () ->
        let list = [ T.Symbol "a"; T.Symbol "b" ] |> list_to_scheme_list in
        let actual = S.Rule_parser.pattern_in_rule (Some list) in
        let expected = Ok ([ P.Constant (T.Symbol "b") ], Some T.Empty_list) in
        Alcotest.(check rule_pp) "simple" expected actual);
    Alcotest.test_case "Rule Parser: parse some symbol pattern" `Quick (fun () ->
        let list = [ T.Symbol "a"; T.Symbol "b"; T.Symbol "c"; T.Number "10" ] |> list_to_scheme_list in
        let actual = S.Rule_parser.pattern_in_rule (Some list) in
        let expected =
          Ok ([ P.Constant (T.Symbol "b"); P.Constant (T.Symbol "c"); P.Constant (T.Number "10") ], Some T.Empty_list)
        in
        Alcotest.(check rule_pp) "simple" expected actual);
    Alcotest.test_case "Rule Parser: parse nested pattern" `Quick (fun () ->
        let list =
          T.Constructor.[ symbol "a"; [ symbol "a"; symbol "b" ] |> list_to_scheme_list ] |> list_to_scheme_list
        in
        let actual = S.Rule_parser.pattern_in_rule (Some list) in
        let expected = Ok ([ P.Nested [ P.Constant (T.Symbol "a"); P.Constant (T.Symbol "b") ] ], Some T.Empty_list) in
        Alcotest.(check rule_pp) "simple" expected actual);
    Alcotest.test_case "Rule Parser: parse pattern contained ellipsis" `Quick (fun () ->
        let list = T.Constructor.[ symbol "a"; symbol "a"; symbol "..." ] |> list_to_scheme_list in
        let actual = S.Rule_parser.pattern_in_rule (Some list) in
        let expected = Ok ([ P.Constant (T.Symbol "a"); P.Constant (T.Symbol "...") ], Some T.Empty_list) in
        Alcotest.(check rule_pp) "simple" expected actual);
    Alcotest.test_case "Rule Parser: parse pattern contained dot" `Quick (fun () ->
        let list = T.Constructor.(cons (symbol "a") @@ cons (symbol "b") (symbol "c")) in
        let actual = S.Rule_parser.pattern_in_rule (Some list) in
        let expected = Ok ([ P.Constant (T.Symbol "b"); P.Dotted (P.Constant (T.Symbol "c")) ], None) in
        Alcotest.(check rule_pp) "simple" expected actual);
  ]

let syntax_rule_parser_test =
  let pattern_p = Alcotest.of_pp S.Pattern.pp in
  let pattern_in_rule_pp = Alcotest.(list pattern_p) in
  let rule_pp = Alcotest.(result (pair (pair pattern_in_rule_pp pattern_p) @@ option exp_pp) string) in
  let module P = S.Pattern in
  [
    Alcotest.test_case "Syntax rule Parser: parse the simplest rule" `Quick (fun () ->
        let list = [ list_to_scheme_list [ T.Symbol "a" ]; T.Number "1" ] |> list_to_scheme_list in
        let actual = S.Rule_parser.syntax_rule (Some list) in
        let expected = Ok (([], P.Constant (T.Number "1")), Some T.Empty_list) in
        Alcotest.(check rule_pp) "simple" expected actual);
    Alcotest.test_case "Syntax rule Parser: parse template contains list" `Quick (fun () ->
        let list =
          [ list_to_scheme_list [ T.Symbol "a" ]; list_to_scheme_list [ T.Symbol "+"; T.Number "1"; T.Symbol "b" ] ]
          |> list_to_scheme_list
        in
        let actual = S.Rule_parser.syntax_rule (Some list) in
        let expected =
          Ok
            ( ([], P.Nested [ P.Constant (T.Symbol "+"); P.Constant (T.Number "1"); P.Constant (T.Symbol "b") ]),
              Some T.Empty_list )
        in
        Alcotest.(check rule_pp) "simple" expected actual);
  ]

let syntax_rules_parser_test =
  let syntax_rules_testable = Alcotest.testable S.(Syntax_rules.pp) ( = ) in
  let test_rule = Alcotest.(pair syntax_rules_testable @@ option exp_pp) in
  let module P = S.Pattern in
  let list = list_to_scheme_list in
  [
    Alcotest.test_case "Syntax rules Parser: parse the simplest list of syntax-rules" `Quick (fun () ->
        let list = [ list []; list [ list [ T.Symbol "s" ]; T.Number "1" ] ] |> list in
        let actual = S.Rule_parser.syntax_rules (Some list) in
        let rule = ([], P.Constant (T.Number "1")) in
        let expected = Ok (S.Syntax_rules.make ~syntax_rules:[ rule ] (), Some T.Empty_list) in
        Alcotest.(check @@ result test_rule string) "simple" expected actual);
  ]

let tests = list_parser_tests @ pattern_parser_test @ syntax_rule_parser_test @ syntax_rules_parser_test
