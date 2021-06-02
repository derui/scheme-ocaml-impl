module T = Ocaml_scheme.Type
module E = Ocaml_scheme.Environment
module Eval = Ocaml_scheme.Eval
module S = Ocaml_scheme.Eval_stack
module C = Ocaml_scheme.Eval_context
module P = Ocaml_scheme.Primitive_op
module Pr = Ocaml_scheme.Printer
module D = Ocaml_scheme.Data_type

let data = Alcotest.testable Pr.pp ( = )

let length_test =
  let error_t = Alcotest.of_pp T.Scheme_error.pp in
  let parse s = Lexing.from_string s |> Ocaml_scheme.Parser.program Ocaml_scheme.Lexer.token |> List.hd in

  [
    Alcotest.test_case "should return 0 if argument is empty list" `Quick (fun () ->
        let actual = P.List_op.length @@ parse "(())" in
        let expected = Ok (T.Number "0") in
        Alcotest.(check @@ result data error_t) "empty" actual expected);
  ]

let tests = length_test
