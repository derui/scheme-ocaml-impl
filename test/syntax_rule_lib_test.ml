module S = Ocaml_scheme.Syntax_rule_lib
module T = Ocaml_scheme.Type
module Pr = Ocaml_scheme.Printer

let exp_pp = Alcotest.testable Pr.pp ( = )

let element_p = Alcotest.(of_pp Pr.pp)

let error_t = Alcotest.testable T.Scheme_error.pp ( = )

let parser_pp p = Alcotest.(result (pair p exp_pp) error_t)

let parse v = Lexing.from_string v |> Ocaml_scheme.Parser.program Ocaml_scheme.Lexer.token |> List.hd

let list_to_scheme_list v = List.rev v |> List.fold_left (fun accum v -> T.cons v accum) T.Empty_list

let list_parser_tests =
  [
    Alcotest.test_case "List Parser: parser a expression from list" `Quick (fun () ->
        let list = [ T.Symbol "foo" ] |> list_to_scheme_list in
        let actual = S.List_parser.element list in
        let expected = Ok (T.Symbol "foo", T.Empty_list) in
        Alcotest.(check @@ parser_pp @@ element_p) "simple" expected actual);
    Alcotest.test_case "List Parser: parser a cdr of list" `Quick (fun () ->
        let list = parse "(a . b)" in
        let actual = S.List_parser.element list in
        let expected = Ok (T.Symbol "a", T.Symbol "b") in
        Alcotest.(check @@ parser_pp element_p) "simple" expected actual);
    Alcotest.test_case "List Parser: many combinator get zero or more repeated list" `Quick (fun () ->
        let p =
          S.List_parser.Infix.(
            (function T.Symbol s -> String.uppercase_ascii s | _ -> "other") <$> S.List_parser.element)
        in
        let list = parse "(foo bar)" in
        let actual = S.List_parser.many p list in
        let expected = Ok ([ "FOO"; "BAR" ], T.Empty_list) in
        Alcotest.(check @@ parser_pp @@ list string) "simple" expected actual);
    Alcotest.test_case "List Parser: many1 combinator get  one or more repeated list" `Quick (fun () ->
        let p =
          S.List_parser.Infix.(
            (function T.Symbol s -> String.uppercase_ascii s | _ -> "other") <$> S.List_parser.element)
        in
        let list = parse "(foo bar)" in
        let actual = S.List_parser.many p list in
        let expected = Ok ([ "FOO"; "BAR" ], T.Empty_list) in
        Alcotest.(check @@ parser_pp @@ list string) "simple" expected actual);
    Alcotest.test_case "List Parser: chainl1 chains operator between parsers" `Quick (fun () ->
        let p =
          S.List_parser.Infix.(
            (function T.Symbol s -> String.uppercase_ascii s | _ -> "other") <$> S.List_parser.element)
        in
        let op = S.List_parser.pure @@ fun a b -> a ^ "___" ^ b in
        let list = [ T.Symbol "foo"; T.Symbol "bar" ] |> list_to_scheme_list in
        let actual = S.List_parser.(chainl1 p op) list in
        let expected = Ok ("FOO___BAR", T.Empty_list) in
        Alcotest.(check @@ parser_pp string) "simple" expected actual);
    Alcotest.test_case "List Parser: expression allow to parse dotted-list" `Quick (fun () ->
        let open S.List_parser.Let_syntax in
        let list = parse "(a . b)" in
        let p =
          let* _ = S.List_parser.element in
          S.List_parser.cdr
        in
        let actual = p list in
        let expected = Ok (T.Symbol "b", T.Empty_list) in
        Alcotest.(check @@ parser_pp @@ element_p) "simple" expected actual);
  ]

let pattern_parser_test =
  let pattern_p = Alcotest.of_pp S.Pattern.pp in
  let rule_pp = Alcotest.(result (pair pattern_p exp_pp) error_t) in
  let module P = S.Pattern in
  [
    Alcotest.test_case "Rule Parser: parse the simplest pattern" `Quick (fun () ->
        let list = [ T.Symbol "a" ] |> list_to_scheme_list in
        let actual = S.Rule_parser.pattern_in_rule list in
        let expected = Ok (P.Nest [], T.Empty_list) in
        Alcotest.(check rule_pp) "simple" expected actual);
    Alcotest.test_case "Rule Parser: parse one symbol pattern" `Quick (fun () ->
        let list = [ T.Symbol "a"; T.Symbol "b" ] |> list_to_scheme_list in
        let actual = S.Rule_parser.pattern_in_rule list in
        let expected = Ok (P.Nest [ P.Symbol "b" ], T.Empty_list) in
        Alcotest.(check rule_pp) "simple" expected actual);
    Alcotest.test_case "Rule Parser: parse some symbol pattern" `Quick (fun () ->
        let list = [ T.Symbol "a"; T.Symbol "b"; T.Symbol "c"; T.Number "10" ] |> list_to_scheme_list in
        let actual = S.Rule_parser.pattern_in_rule list in
        let expected = Ok (P.Nest [ P.Symbol "b"; P.Symbol "c"; P.Constant (T.Number "10") ], T.Empty_list) in
        Alcotest.(check rule_pp) "simple" expected actual);
    Alcotest.test_case "Rule Parser: parse nested pattern" `Quick (fun () ->
        let list = parse "(a (a b))" in
        let actual = S.Rule_parser.pattern_in_rule list in
        let expected = Ok (P.Nest [ P.Nest [ P.Symbol "a"; P.Symbol "b" ] ], T.Empty_list) in
        Alcotest.(check rule_pp) "simple" expected actual);
    Alcotest.test_case "Rule Parser: parse pattern contained ellipsis" `Quick (fun () ->
        let list = parse "(a a ...)" in
        let actual = S.Rule_parser.pattern_in_rule list in
        let expected = Ok (P.Nest [ P.Symbol "a"; P.Symbol "..." ], T.Empty_list) in
        Alcotest.(check rule_pp) "simple" expected actual);
    Alcotest.test_case "Rule Parser: parse pattern contained dot" `Quick (fun () ->
        let list = parse "(a b . c)" in
        let actual = S.Rule_parser.pattern_in_rule list in
        let expected = Ok (P.Nest_dot ([ P.Symbol "b" ], P.Symbol "c"), T.Empty_list) in
        Alcotest.(check rule_pp) "simple" expected actual);
  ]

let syntax_rule_parser_test =
  let pattern_p = Alcotest.of_pp S.Pattern.pp in
  let rule_pp = Alcotest.(result (pair (pair pattern_p exp_pp) exp_pp) error_t) in
  let module P = S.Pattern in
  [
    Alcotest.test_case "Syntax rule Parser: parse the simplest rule" `Quick (fun () ->
        let list = [ list_to_scheme_list [ T.Symbol "a" ]; T.Number "1" ] |> list_to_scheme_list in
        let actual = S.Rule_parser.syntax_rule list in
        let expected = Ok ((P.Nest [], T.Number "1"), T.Empty_list) in
        Alcotest.(check rule_pp) "simple" expected actual);
    Alcotest.test_case "Syntax rule Parser: parse template contains list" `Quick (fun () ->
        let list =
          [ list_to_scheme_list [ T.Symbol "a" ]; list_to_scheme_list [ T.Symbol "+"; T.Number "1"; T.Symbol "b" ] ]
          |> list_to_scheme_list
        in
        let actual = S.Rule_parser.syntax_rule list in
        let expected = Ok ((P.Nest [], parse "(+ 1 b)"), T.Empty_list) in
        Alcotest.(check rule_pp) "simple" expected actual);
  ]

let syntax_rules_parser_test =
  let syntax_rules_testable = Alcotest.testable S.(Syntax_rules.pp) ( = ) in
  let test_rule = Alcotest.(pair syntax_rules_testable exp_pp) in
  let module P = S.Pattern in
  let list = list_to_scheme_list in
  [
    Alcotest.test_case "Syntax rules Parser: parse the simplest list of syntax-rules" `Quick (fun () ->
        let list = [ list []; list [ list [ T.Symbol "s" ]; T.Number "1" ] ] |> list in
        let actual = S.Rule_parser.syntax_rules list in
        let rule = (P.Nest [], T.Number "1") in
        let expected = Ok (S.Syntax_rules.make ~syntax_rules:[ rule ] () |> Result.get_ok, T.Empty_list) in
        Alcotest.(check @@ result test_rule error_t) "simple" expected actual);
  ]

let syntax_rule_test =
  let parse str = Lexing.from_string str |> Ocaml_scheme.(Parser.program Ocaml_scheme.Lexer.token) |> List.hd in
  let module P = S.Pattern in
  [
    Alcotest.test_case "Syntax rule: no pattern variable if pattern is not contains symbol" `Quick (fun () ->
        let list = "((a 1 2 3) 5)" |> parse in
        let actual =
          S.Rule_parser.syntax_rule list |> Result.map (fun (v, _) -> S.Syntax_rule.pattern_variables [] v)
        in
        let expected = Ok [] in
        Alcotest.(check @@ result (list @@ pair int string) error_t) "simple" expected actual);
    Alcotest.test_case "Syntax rule: allow nest in template" `Quick (fun () ->
        let list = "((_ a b c) (+ (+ a b) (* b c)))" |> parse in
        let actual = S.Rule_parser.syntax_rule list |> Result.map (fun v -> fst v |> snd) in
        let expected = Ok (parse "(+ (+ a b) (* b c))") in
        Alcotest.(check @@ result element_p error_t) "nested" expected actual);
    Alcotest.test_case "Syntax rule: have level 0 pattern variable " `Quick (fun () ->
        let list = "((_ b 2 3) 5)" |> parse in
        let actual =
          S.Rule_parser.syntax_rule list |> Result.map (fun (v, _) -> S.Syntax_rule.pattern_variables [] v)
        in
        let expected = Ok [ (0, "b") ] in
        Alcotest.(check @@ result (list @@ pair int string) error_t) "simple" expected actual);
    Alcotest.test_case "Syntax rule: have level 0 pattern variable with nested" `Quick (fun () ->
        let list = "((_ b (c d) e) 5)" |> parse in
        let actual =
          S.Rule_parser.syntax_rule list
          |> Result.map (fun (v, _) -> S.Syntax_rule.pattern_variables [] v)
          |> Result.map (List.sort Stdlib.compare)
        in
        let expected = Ok ([ (0, "b"); (0, "c"); (0, "d"); (0, "e") ] |> List.sort Stdlib.compare) in
        Alcotest.(check @@ result (list @@ pair int string) error_t) "simple" expected actual);
    Alcotest.test_case "Syntax rule: have level 1 pattern variable with ellipsis" `Quick (fun () ->
        let rule = (P.(Nest_ellipsis ([ P.Symbol "a" ], P.Symbol "b", [])), T.Symbol "c") in
        let actual = rule |> S.Syntax_rule.pattern_variables [] |> List.sort Stdlib.compare in
        let expected = [ (0, "a"); (1, "b") ] |> List.sort Stdlib.compare in
        Alcotest.(check @@ list @@ pair int string) "simple" expected actual);
    Alcotest.test_case "Syntax rule: have pattern variable that have level greater than 1" `Quick (fun () ->
        let rule =
          (P.(Nest_ellipsis ([ P.Symbol "a" ], Nest_ellipsis ([ Symbol "b" ], Symbol "c", []), [])), T.Symbol "c")
        in
        let actual = rule |> S.Syntax_rule.pattern_variables [] |> List.sort Stdlib.compare in
        let expected = [ (0, "a"); (1, "b"); (2, "c") ] |> List.sort Stdlib.compare in
        Alcotest.(check @@ list @@ pair int string) "simple" expected actual);
  ]

let syntax_rules_test =
  let test_syntax_rules = Alcotest.testable S.(Syntax_rules.pp) ( = ) in
  let pattern_t = Alcotest.of_pp S.Pattern.pp in
  let rule_t = Alcotest.(pair pattern_t exp_pp) in
  let parse str = Lexing.from_string str |> Ocaml_scheme.(Parser.program Ocaml_scheme.Lexer.token) |> List.hd in
  let module P = S.Pattern in
  [
    Alcotest.test_case "Syntax rules: parse and create simple pattern" `Quick (fun () ->
        let list = "(() ((a 1 2 3) 5))" |> parse in

        let actual = S.Rule_parser.syntax_rules list |> Result.map fst in
        let syntax_rules = [ "((a 1 2 3) 5)" |> parse |> S.Rule_parser.syntax_rule |> Result.get_ok |> fst ] in
        let expected = S.Syntax_rules.make ~syntax_rules () in
        Alcotest.(check @@ result test_syntax_rules error_t) "simple" expected actual);
    Alcotest.test_case "Syntax rules: create patterns that are contained ellipsis" `Quick (fun () ->
        let list = "(() ((_ b ... 2 3) 5))" |> parse in
        let actual = S.Rule_parser.syntax_rules list |> Result.map fst in
        let syntax_rules = [ "((_ b ... 2 3) 5)" |> parse |> S.Rule_parser.syntax_rule |> Result.get_ok |> fst ] in
        let expected = S.Syntax_rules.make ~syntax_rules () in
        Alcotest.(check @@ result test_syntax_rules error_t) "simple" expected actual);
    Alcotest.test_case "Syntax rules: order of rules should be same as appearance order" `Quick (fun () ->
        let list = "(() ((_ a) 5) ((_ b) 6))" |> parse in
        let parse_rule v = parse v |> S.Rule_parser.syntax_rule |> Result.get_ok |> fst in
        let expected = [ "((_ a) 5)" |> parse_rule; "((_ b) 6)" |> parse_rule ] in
        let actual = S.Rule_parser.syntax_rules list |> Result.map fst |> Result.get_ok in
        let v l n = List.nth l.S.Syntax_rules.syntax_rules n in

        Alcotest.(check rule_t) "simple" (List.nth expected 0) (v actual 0);
        Alcotest.(check rule_t) "simple" (List.nth expected 1) (v actual 1));
    Alcotest.test_case "Syntax rules: validate template if have unaided ellipsis" `Quick (fun () ->
        let list = "(() ((_ b ) (...)))" |> parse in
        let actual = S.Rule_parser.syntax_rules list |> Result.map fst in
        let expected = T.raise_syntax_error "empty: ()" in
        Alcotest.(check @@ result test_syntax_rules error_t) "simple" expected actual);
    Alcotest.test_case "Syntax rules: allow ellipsis literal" `Quick (fun () ->
        let list = "(() ((_ b ) (... ...)))" |> parse in
        let actual = S.Rule_parser.syntax_rules list |> Result.map fst in
        let syntax_rules = [ "((_ b) (... ...))" |> parse |> S.Rule_parser.syntax_rule |> Result.get_ok |> fst ] in
        let expected = S.Syntax_rules.make ~syntax_rules () in
        Alcotest.(check @@ result test_syntax_rules error_t) "simple" expected actual);
    Alcotest.test_case "Syntax rules: complex nested template" `Quick (fun () ->
        let list = "(() ((_ a) (- (+ 1 a) (* 2 a))))" |> parse in
        let parse_rule v = parse v |> S.Rule_parser.syntax_rule |> Result.map fst in
        let expected = "((_ a) (- (+ 1 a) (* 2 a)))" |> parse_rule in
        let actual = S.Rule_parser.syntax_rules list |> Result.map fst in
        let v l n = Result.map (fun l -> List.nth l.S.Syntax_rules.syntax_rules n) l in

        Alcotest.(check @@ result rule_t error_t) "simple" expected (v actual 0));
  ]

let tests =
  list_parser_tests @ pattern_parser_test @ syntax_rule_parser_test @ syntax_rules_parser_test @ syntax_rule_test
  @ syntax_rules_test
