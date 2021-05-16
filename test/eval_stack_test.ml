module P = Ocaml_scheme.Parser
module L = Ocaml_scheme.Lexer
module T = Ocaml_scheme.Type
module D = Ocaml_scheme.Data_type
module C = Ocaml_scheme.Eval_stack
module Pr = Ocaml_scheme.Printer

let exp = Alcotest.testable Pr.pp ( = )

let parse str = Lexing.from_string str |> P.program L.token |> List.hd

let to_list = Ocaml_scheme.Internal_lib.list_to_scheme_list

let tests =
  [
    Alcotest.test_case "store evaluated value" `Quick (fun () ->
        let context = C.make () in
        let actual = C.push_value context ~value:(T.Number "12") |> C.evaluated_values in
        let expected = [ T.Number "12" ] |> to_list in
        Alcotest.(check @@ exp) "result" actual expected);
    Alcotest.test_case "allow to initialize value" `Quick (fun () ->
        let context = C.make ~evaluated_values:(T.Cons (T.Symbol "a", T.Empty_list)) () in
        let actual = C.push_value context ~value:(T.Number "12") |> C.evaluated_values in
        let expected = [ T.Number "12"; T.Symbol "a" ] |> to_list in
        Alcotest.(check @@ exp) "result" actual expected);
  ]
