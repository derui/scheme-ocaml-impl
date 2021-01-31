module T = Ocaml_scheme.Type
module E = Ocaml_scheme.Environment
module Eval = Ocaml_scheme.Eval
module P = Ocaml_scheme.Primitive_op
module Pr = Ocaml_scheme.Printer

let data = Alcotest.testable Pr.pp ( = )

let tests =
  [
    Alcotest.test_case "should return same value if data is number" `Quick (fun () ->
        let env = E.make [] in
        let actual = Eval.eval env (T.Number "15") in
        let expected = Ok (T.Number "15") in
        Alcotest.(check @@ result data string) "number" actual expected);
    Alcotest.test_case "should return same value if data is boolean" `Quick (fun () ->
        let env = E.make [] in
        let actual = Eval.eval env T.False in
        Alcotest.(check @@ result data string) "false" actual (Ok T.False);
        let actual = Eval.eval env T.True in
        Alcotest.(check @@ result data string) "true" actual (Ok T.True));
    Alcotest.test_case "should apply function" `Quick (fun () ->
        let env = E.make [ ("+", T.Value (T.Primitive_fun P.Number_op.Export.plus)) ] in
        let actual =
          Eval.eval env (T.Cons (T.Symbol "+", T.Cons (T.Number "3", T.Cons (T.Number "4", T.Empty_list))))
        in
        Alcotest.(check @@ result data string) "false" actual (Ok (T.Number "7")));
  ]
