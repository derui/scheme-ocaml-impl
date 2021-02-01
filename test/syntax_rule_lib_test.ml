module S = Ocaml_scheme.Syntax_rule_lib
module T = Ocaml_scheme.Type
module Pr = Ocaml_scheme.Printer

let exp_pp = Alcotest.testable Pr.pp ( = )

let parser_pp = Alcotest.(result (pair exp_pp @@ exp_pp) string)

let list_to_scheme_list v = List.rev v |> List.fold_left (fun accum v -> T.Cons (v, accum)) T.Empty_list

let rule_parser_tests =
  [
    Alcotest.test_case "Rule Parser: parser a expression from list" `Quick (fun () ->
        let list = [ T.Symbol "foo" ] |> list_to_scheme_list in
        let actual = S.Rule_parser.get_exp list in
        let expected = Ok (T.Symbol "foo", T.Empty_list) in
        Alcotest.(check parser_pp) "simple" actual expected);
  ]

let tests = rule_parser_tests
