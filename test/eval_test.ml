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
  let to_scheme_string str =
    T.Scheme_string (str |> String.to_seq |> Seq.map Char.escaped |> Seq.map D.Scheme_char.of_string |> List.of_seq)
  in

  [
    Alcotest.test_case "should return same value if data is number" `Quick (fun () ->
        let env = E.make [] in
        let actual = Eval.eval ~env @@ parse "15" in
        let expected = Ok (T.Number "15") in
        Alcotest.(check @@ result data error_t) "number" actual expected);
    Alcotest.test_case "should return same value if data is string" `Quick (fun () ->
        let env = E.make [] in
        let actual = Eval.eval ~env @@ parse "\"string\"" in
        let expected = Ok (to_scheme_string "string") in
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
    Alcotest.test_case "should be able to evaluate if-syntax" `Quick (fun () ->
        let env = E.make [ ("+", T.Primitive_fun P.Number_op.Export.plus); ("if", T.Syntax T.S_if) ] in
        let actual = Eval.eval ~env @@ parse "(if #t (+ 3 4) 0)" in
        Alcotest.(check @@ result data error_t) "false" actual (Ok (T.Number "7"));
        let actual = Eval.eval ~env @@ parse "(if #f (+ 3 4) 0)" in
        Alcotest.(check @@ result data error_t) "false" actual (Ok (T.Number "0")));
    Alcotest.test_case "should be able to evaluate define-syntax" `Quick (fun () ->
        let env = E.make [ ("+", T.Primitive_fun P.Number_op.Export.plus); ("define", T.Syntax T.S_define) ] in
        let actual = Eval.eval ~env @@ parse "(define b 4)" in
        Alcotest.(check @@ result data error_t) "false" actual (Ok (T.Number "4"));
        let v = E.get env ~key:"b" in
        Alcotest.(check @@ option data) "false" v (Some (T.Number "4"));
        let _ = Eval.eval ~env @@ parse "(define b (+ 4 5))" in
        let actual = E.get env ~key:"b" in
        Alcotest.(check @@ option data) "false" actual (Some (T.Number "9")));
    Alcotest.test_case "should be able to evaluate set!-syntax" `Quick (fun () ->
        let env =
          E.make
            [
              ("+", T.Primitive_fun P.Number_op.Export.plus);
              ("define", T.Syntax T.S_define);
              ("set!", T.Syntax T.S_set_force);
            ]
        in
        let _ = Eval.eval ~env @@ parse "(define b 4)" in
        let actual = Eval.eval ~env @@ parse "(set! b 5)" in
        Alcotest.(check @@ result data error_t) "false" actual (Ok (T.Number "5"));
        let v = E.get env ~key:"b" in
        Alcotest.(check @@ option data) "false" v (Some (T.Number "5")));
    Alcotest.test_case "should be able to evaluate quote-syntax" `Quick (fun () ->
        let env = E.make [ ("quote", T.Syntax T.S_quote) ] in
        let actual = Eval.eval ~env @@ parse "'(1 \"foo\")" in
        Alcotest.(check @@ result data error_t)
          "false" actual
          (Ok (T.Cons (T.Number "1", T.Cons (to_scheme_string "foo", T.Empty_list)))));
    Alcotest.test_case "should be able to evaluate quasiquote-syntax" `Quick (fun () ->
        let env = E.make [ ("quasiquote", T.Syntax T.S_quote) ] in
        let actual = Eval.eval ~env @@ parse "`(1 \"foo\")" in
        Alcotest.(check @@ result data error_t)
          "false" actual
          (Ok (T.Cons (T.Number "1", T.Cons (to_scheme_string "foo", T.Empty_list)))));
  ]
