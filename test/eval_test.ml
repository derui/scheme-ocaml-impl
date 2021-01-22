module S = Ocaml_scheme.Syntax
module E = Ocaml_scheme.Environment
module Eval = Ocaml_scheme.Eval
module B = Ocaml_scheme.Buildin_op

let data = Alcotest.testable S.data_pp ( = )

let tests =
  [
    Alcotest.test_case "should return same value if data is number" `Quick (fun () ->
        let env = E.make [] in
        let actual = Eval.eval env (S.Number "15") in
        let expected = Ok (S.Number "15") in
        Alcotest.(check @@ result data string) "number" actual expected);
    Alcotest.test_case "should return same value if data is boolean" `Quick (fun () ->
        let env = E.make [] in
        let actual = Eval.eval env S.False in
        Alcotest.(check @@ result data string) "false" actual (Ok S.False);
        let actual = Eval.eval env S.True in
        Alcotest.(check @@ result data string) "true" actual (Ok S.True));
    Alcotest.test_case "should apply function" `Quick (fun () ->
        let env = E.make [ ("+", S.Value (S.Native_fun B.Number_op.Export.plus)) ] in
        let actual =
          Eval.eval env (S.Cons (S.Symbol "+", S.Cons (S.Number "3", S.Cons (S.Number "4", S.Empty_list))))
        in
        Alcotest.(check @@ result data string) "false" actual (Ok (S.Number "7")));
  ]
