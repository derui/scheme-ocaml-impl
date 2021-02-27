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
        let syntax_rules = parse_syntax_rules "(() ((_) 1))" in
        let data = parse "()" in
        let actual = M.match_syntax_rules ~syntax_rules ~data in
        let expected = Some (M.Pattern_matcher.make (), List.hd syntax_rules.syntax_rules) in

        Alcotest.(check result_t) "matched" expected actual);
  ]

let tests = matcher_tests
