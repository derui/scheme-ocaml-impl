module S = Ocaml_scheme.Syntax_rule_lib
module M = Ocaml_scheme.Syntax_rule_pattern_matcher
module T = Ocaml_scheme.Type
module Pr = Ocaml_scheme.Printer

let matcher_t = Alcotest.testable M.Pattern_matcher.pp ( = )

let syntax_rule_t = Alcotest.testable S.(Syntax_rule.pp) ( = )

let result_t = Alcotest.(option @@ pair matcher_t syntax_rule_t)

let parse str = Lexing.from_string str |> Ocaml_scheme.(Parser.program Ocaml_scheme.Lexer.token) |> List.hd

let parse_syntax_rules str =
  let v = parse str in
  S.Rule_parser.syntax_rules v |> Result.get_ok |> fst

let matcher_tests =
  [
    Alcotest.test_case "match the simplest pattern" `Quick (fun () ->
        let syntax_rules = parse_syntax_rules "((_) ((_) 1))" in
        let data = parse "()" in
        let actual = M.match_syntax_rules ~syntax_rules ~data in
        let expected = Some (M.Pattern_matcher.make (), List.hd syntax_rules.syntax_rules) in

        Alcotest.(check result_t) "matched" expected actual);
    Alcotest.test_case "match the pattern have a pattern variable" `Quick (fun () ->
        let syntax_rules = parse_syntax_rules "(() ((_ a) 1))" in
        let data = parse "(1)" in
        let actual = M.match_syntax_rules ~syntax_rules ~data in
        let expected = M.Pattern_matcher.(make () |> put_pattern_variable "a" (T.Number "1")) in
        let expected = Some (expected, List.hd syntax_rules.syntax_rules) in

        Alcotest.(check result_t) "matched" expected actual);
    Alcotest.test_case "a pattern variable should be able to have any data" `Quick (fun () ->
        let syntax_rules = parse_syntax_rules "(() ((_ a) 1))" in
        let data = parse "((a b c))" in
        let actual = M.match_syntax_rules ~syntax_rules ~data in
        let expected =
          M.Pattern_matcher.(
            make ()
            |> put_pattern_variable "a"
                 T.Constructor.(cons (symbol "a") @@ cons (symbol "b") @@ cons (symbol "c") T.Empty_list))
        in
        let expected = Some (expected, List.hd syntax_rules.syntax_rules) in

        Alcotest.(check result_t) "matched" expected actual);
    Alcotest.test_case "should not match if number of variable is not match number of element of given list" `Quick
      (fun () ->
        let syntax_rules = parse_syntax_rules "(() ((_ a) 1))" in
        let data = parse "(a b)" in
        let actual = M.match_syntax_rules ~syntax_rules ~data in

        Alcotest.(check result_t) "matched" None actual);
    Alcotest.test_case "should match if any syntax rule matches given list" `Quick (fun () ->
        let syntax_rules = parse_syntax_rules "(() ((_ a) 1) ((_ a b) 2))" in
        let data = parse "(b a)" in
        let expected =
          M.Pattern_matcher.(
            make () |> put_pattern_variable "a" (T.Symbol "b") |> put_pattern_variable "b" (T.Symbol "a"))
        in

        let expected = Some (expected, List.nth syntax_rules.syntax_rules 1) in
        let actual = M.match_syntax_rules ~syntax_rules ~data in

        Alcotest.(check result_t) "matched" expected actual);
  ]

let ellipsis_matcher_tests =
  [
    Alcotest.test_case "match the simplest ellipsis pattern" `Quick (fun () ->
        let syntax_rules = parse_syntax_rules "(() ((_ a ...) 1))" in
        let data = parse "()" in
        let actual = M.match_syntax_rules ~syntax_rules ~data in
        let expected = Some (M.Pattern_matcher.make (), List.hd syntax_rules.syntax_rules) in

        Alcotest.(check result_t) "matched" expected actual);
    Alcotest.test_case "match the simplest ellipsis pattern" `Quick (fun () ->
        let syntax_rules = parse_syntax_rules "(() ((_ a ...) 1))" in
        let data = parse "(1 2 3)" in
        let actual = M.match_syntax_rules ~syntax_rules ~data in
        let leveled =
          [
            M.Pattern_matcher.(make () |> put_pattern_variable "a" (T.Number "1"));
            M.Pattern_matcher.(make () |> put_pattern_variable "a" (T.Number "2"));
            M.Pattern_matcher.(make () |> put_pattern_variable "a" (T.Number "3"));
          ]
        in
        let expected = Some (M.Pattern_matcher.(make () |> set_level leveled), List.hd syntax_rules.syntax_rules) in

        Alcotest.(check result_t) "matched" expected actual);
    Alcotest.test_case "match the ellipsis pattern that have before pattern" `Quick (fun () ->
        let syntax_rules = parse_syntax_rules "(() ((_ b a ...) 1))" in
        let data = parse "(1 2 3)" in
        let actual = M.match_syntax_rules ~syntax_rules ~data in
        let leveled =
          [
            M.Pattern_matcher.(make () |> put_pattern_variable "a" (T.Number "2"));
            M.Pattern_matcher.(make () |> put_pattern_variable "a" (T.Number "3"));
          ]
        in
        let expected =
          Some
            ( M.Pattern_matcher.(make () |> set_level leveled |> put_pattern_variable "b" (T.Number "1")),
              List.hd syntax_rules.syntax_rules )
        in

        Alcotest.(check result_t) "matched" expected actual);
    Alcotest.test_case "match the ellipsis pattern that have after pattern" `Quick (fun () ->
        let syntax_rules = parse_syntax_rules "(() ((_ a ... b) 1))" in
        let data = parse "(1 2 3)" in
        let actual = M.match_syntax_rules ~syntax_rules ~data in
        let leveled =
          [
            M.Pattern_matcher.(make () |> put_pattern_variable "a" (T.Number "1"));
            M.Pattern_matcher.(make () |> put_pattern_variable "a" (T.Number "2"));
          ]
        in
        let expected =
          Some
            ( M.Pattern_matcher.(make () |> set_level leveled |> put_pattern_variable "b" (T.Number "3")),
              List.hd syntax_rules.syntax_rules )
        in

        Alcotest.(check result_t) "matched" expected actual);
    Alcotest.test_case "match a pattern variable as a list" `Quick (fun () ->
        let syntax_rules = parse_syntax_rules "(() ((_ a b) 1))" in
        let data = parse "((1 2) 3)" in
        let actual = M.match_syntax_rules ~syntax_rules ~data in
        let expected =
          Some
            ( M.Pattern_matcher.(
                make () |> put_pattern_variable "b" (T.Number "3") |> put_pattern_variable "a" (parse "(1 2)")),
              List.hd syntax_rules.syntax_rules )
        in

        Alcotest.(check result_t) "matched" expected actual);
  ]

let tests = matcher_tests @ ellipsis_matcher_tests
