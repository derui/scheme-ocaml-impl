module T = Ocaml_scheme.Type
module E = Ocaml_scheme.Environment
module Eval = Ocaml_scheme.Eval
module S = Ocaml_scheme.Eval_stack
module C = Ocaml_scheme.Eval_context
module P = Ocaml_scheme.Primitive_op
module Pr = Ocaml_scheme.Printer
module D = Ocaml_scheme.Data_type

let data = Alcotest.testable Pr.pp ( = )

let tests =
  let error_t = Alcotest.of_pp T.Scheme_error.pp in
  let parse s = Lexing.from_string s |> Ocaml_scheme.Parser.program Ocaml_scheme.Lexer.token |> List.hd in

  [
    Alcotest.test_case "should return same value if data is number" `Quick (fun () ->
        let env = E.make [] in
        let actual = Eval.eval ~env @@ parse "15" in
        let expected = Ok (T.Number "15") in
        Alcotest.(check @@ result data error_t) "number" actual expected);
    Alcotest.test_case "should return same value if data is string" `Quick (fun () ->
        let env = E.make [] in
        let actual = Eval.eval ~env @@ parse "\"string\"" in
        let expected =
          Ok
            (T.Scheme_string
               ("string" |> String.to_seq |> Seq.map Char.escaped |> Seq.map D.Scheme_char.of_string |> List.of_seq))
        in
        Alcotest.(check @@ result data error_t) "string" actual expected);
    Alcotest.test_case "should return same value if data is boolean" `Quick (fun () ->
        let env = E.make [] in
        let actual = Eval.eval ~env @@ parse "#f" in
        Alcotest.(check @@ result data error_t) "false" actual (Ok T.False);
        let actual = Eval.eval ~env @@ parse "#t" in
        Alcotest.(check @@ result data error_t) "true" actual (Ok T.True));
    Alcotest.test_case "should be able to apply function" `Quick (fun () ->
        let env = E.make [ ("+", T.Primitive_fun P.Number_op.Export.plus) ] in
        let actual = Eval.eval ~env @@ parse "(+ 3 4)" in
        Alcotest.(check @@ result data error_t) "false" actual (Ok (T.Number "7")));
    Alcotest.test_case "should be able to evaluate nested application" `Quick (fun () ->
        let env = E.make [ ("+", T.Primitive_fun P.Number_op.Export.plus) ] in
        let actual = Eval.eval ~env @@ parse "(+ (+ 1 2) (+ 3 5))" in
        Alcotest.(check @@ result data error_t) "false" actual (Ok (T.Number "11")));
    Alcotest.test_case "should be able to parse if-syntax" `Quick (fun () ->
        let env = E.make [ ("+", T.Primitive_fun P.Number_op.Export.plus); ("if", T.Syntax T.S_if) ] in
        let actual = Eval.eval ~env @@ parse "(if #t (+ 3 4) 0)" in
        Alcotest.(check @@ result data error_t) "false" actual (Ok (T.Number "7"));
        let actual = Eval.eval ~env @@ parse "(if #f (+ 3 4) 0)" in
        Alcotest.(check @@ result data error_t) "false" actual (Ok (T.Number "0")));
  ]
