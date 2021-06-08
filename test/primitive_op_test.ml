module T = Ocaml_scheme.Type
module E = Ocaml_scheme.Environment
module Eval = Ocaml_scheme.Eval
module S = Ocaml_scheme.Eval_stack
module C = Ocaml_scheme.Eval_context
module P = Ocaml_scheme.Primitive_op
module Pr = Ocaml_scheme.Printer
module D = Ocaml_scheme.Data_type

let data = Alcotest.testable Pr.pp ( = )

let error_t = Alcotest.of_pp T.Scheme_error.pp

let parse s = Lexing.from_string s |> Ocaml_scheme.Parser.program Ocaml_scheme.Lexer.token |> List.hd

let c _ = ()

let length_test =
  [
    Alcotest.test_case "should return 0 if argument is empty list" `Quick (fun () ->
        let actual = P.List_op.length c @@ parse "(())" in
        let expected = Ok (T.Number "0") in
        Alcotest.(check @@ result data error_t) "empty" actual expected);
  ]

let set_cdr_test =
  [
    Alcotest.test_case "should update cdr in place" `Quick (fun () ->
        let target = parse "(1 2)" in
        let actual = P.List_op.set_cdr c @@ T.(cons target @@ cons (Number "3") Empty_list) in
        let expected = Ok T.Undef in
        Alcotest.(check @@ result data error_t) "empty" actual expected;
        Alcotest.(check @@ data) "updated" (parse "(1 2 . 3)") target);
  ]

let append_test =
  [
    Alcotest.test_case "should return empty list if no any arguments" `Quick (fun () ->
        let actual = P.List_op.append c @@ parse "(())" in
        let expected = Ok T.Empty_list in
        Alcotest.(check @@ result data error_t) "empty" expected actual);
    Alcotest.test_case "should return first argument if it has only one argument" `Quick (fun () ->
        let actual = P.List_op.append c @@ parse "(3)" in
        let expected = Ok (T.Number "3") in
        Alcotest.(check @@ result data error_t) "empty" expected actual);
    Alcotest.test_case "should return appended list " `Quick (fun () ->
        let actual = P.List_op.append c @@ parse "((3 4) (5 6))" in
        let expected = Ok (parse "(3 4 5 6)") in
        Alcotest.(check @@ result data error_t) "empty" expected actual);
  ]

let tests = length_test @ append_test
