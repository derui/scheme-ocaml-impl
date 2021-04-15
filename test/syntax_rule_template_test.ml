module S = Ocaml_scheme.Syntax_rule_lib
module M = Ocaml_scheme.Syntax_rule_pattern_matcher
module Pr = Ocaml_scheme.Printer
module T = Ocaml_scheme.Syntax_rule_template

let matcher_t = Alcotest.testable M.Pattern_matcher.pp ( = )

let syntax_rule_t = Alcotest.testable S.(Syntax_rule.pp) ( = )

let data_t = Alcotest.testable Pr.pp ( = )

let result_t = Alcotest.(option @@ pair matcher_t syntax_rule_t)

let parse str = Lexing.from_string str |> Ocaml_scheme.(Parser.program Ocaml_scheme.Lexer.token) |> List.hd

let parse_rule str =
  let v = parse str |> S.Rule_parser.syntax_rules |> Result.map fst in
  match v with Error _ -> Alcotest.fail "failed" | Ok v -> v

let test_cases =
  [
    ("(() ((_ a b) (a a b b)))", "(1 2)", "(1 1 2 2)");
    ("(() ((_ a b) a))", "(1 2)", "1");
    ("(() ((_ a b) ((a) b)))", "(1 2)", "((1) 2)");
    ("(() ((_ a) (+ (* a a) (- a 1))))", "(2)", "(+ (* 2 2) (- 2 1))");
  ]

let to_test (rules, list, expanded) =
  Alcotest.test_case (Printf.sprintf "expand template: (%s, %s)" rules expanded) `Quick (fun () ->
      let rules = parse_rule rules in
      let list = parse list in
      let expected = parse expanded in
      let matcher, (_, template) = M.match_syntax_rules ~syntax_rules:rules ~data:list |> Option.get in
      let actual = T.expand matcher rules.S.Syntax_rules.ellipsis template in
      Alcotest.(check data_t) "expanded" expected actual)

let tests = List.map to_test test_cases
