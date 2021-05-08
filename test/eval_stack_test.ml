module P = Ocaml_scheme.Parser
module L = Ocaml_scheme.Lexer
module T = Ocaml_scheme.Type
module D = Ocaml_scheme.Data_type
module C = Ocaml_scheme.Eval_stack
module Pr = Ocaml_scheme.Printer

let exp = Alcotest.testable Pr.pp ( = )

let parse str = Lexing.from_string str |> P.program L.token |> List.hd

let to_list = Ocaml_scheme.Internal_lib.list_to_scheme_list

let expression_test =
  let of_context source = parse source |> C.make ~kind:In_expression in
  [
    Alcotest.test_case "kind of context" `Quick (fun () ->
        let context = of_context "(a b)" in
        Alcotest.(check @@ of_pp Fmt.nop) "exp" (C.kind context) C.In_expression);
    Alcotest.test_case "expression of context" `Quick (fun () ->
        let context = of_context "(a b)" in
        let actual = C.expression context in
        let expected = to_list [ T.Symbol "a"; T.Symbol "b" ] in
        Alcotest.(check @@ exp) "exp" actual expected);
    Alcotest.test_case "get current expression to evaluate in evaluation context" `Quick (fun () ->
        let context = of_context "(a b)" in
        let actual = C.current context in
        let expected = T.Symbol "a" |> Option.some in
        Alcotest.(check @@ option exp) "empty list" actual expected);
    Alcotest.test_case "store evaluated value" `Quick (fun () ->
        let context = of_context "(a b)" in
        let actual = C.push_value context ~value:(T.Number "12") |> C.evaluated_values in
        let expected = [ T.Number "12" ] |> to_list in
        Alcotest.(check @@ exp) "result" actual expected);
    Alcotest.test_case "forward indicator when store evaluated value" `Quick (fun () ->
        let context = of_context "(a b)" in
        let actual = C.push_value context ~value:(T.Number "12") |> C.current in
        let expected = Some (T.Symbol "b") in
        Alcotest.(check @@ option exp) "result" actual expected);
    Alcotest.test_case "can not get expression if forward last expression" `Quick (fun () ->
        let context = of_context "(a b)" in
        let actual =
          C.push_value context ~value:(T.Number "12")
          |> C.push_value ~value:(T.Number "13") |> C.push_value ~value:T.True |> C.current
        in
        Alcotest.(check @@ option exp) "result" actual None);
    Alcotest.test_case "do not indicate in nested expressions" `Quick (fun () ->
        let context = of_context "((a b) c)" |> C.push_value ~value:T.True in
        let actual = C.current context in
        let expected = Some (parse "c") in
        Alcotest.(check @@ option exp) "empty list" actual expected);
  ]

let closure_test =
  let of_context source = parse source |> C.make ~kind:In_closure in
  [
    Alcotest.test_case "kind of context" `Quick (fun () ->
        let context = of_context "(a b)" in
        Alcotest.(check @@ of_pp Fmt.nop) "exp" (C.kind context) C.In_closure);
    Alcotest.test_case "body of context" `Quick (fun () ->
        let context = of_context "(a b)" in
        let actual = C.expression context in
        let expected = to_list [ T.Symbol "a"; T.Symbol "b" ] in
        Alcotest.(check @@ exp) "exp" actual expected);
    Alcotest.test_case "get current expression to evaluate in closure body" `Quick (fun () ->
        let context = of_context "(a b)" in
        let actual = C.current context in
        let expected = T.Symbol "a" |> Option.some in
        Alcotest.(check @@ option exp) "empty list" actual expected);
    Alcotest.test_case "store evaluated value" `Quick (fun () ->
        let context = of_context "(a b)" in
        let actual = C.push_value context ~value:(T.Number "12") |> C.evaluated_values in
        let expected = T.Number "12" in
        Alcotest.(check @@ exp) "result" actual expected);
    Alcotest.test_case "store evaluated value only pushed last" `Quick (fun () ->
        let context = of_context "(a b)" in
        let actual =
          C.push_value context ~value:(T.Number "12") |> C.push_value ~value:(T.Symbol "b") |> C.evaluated_values
        in
        let expected = T.Symbol "b" in
        Alcotest.(check @@ exp) "result" actual expected);
    Alcotest.test_case "forward indicator when store evaluated value" `Quick (fun () ->
        let context = of_context "(a b)" in
        let actual = C.push_value context ~value:(T.Number "12") |> C.current in
        let expected = Some (T.Symbol "b") in
        Alcotest.(check @@ option exp) "result" actual expected);
    Alcotest.test_case "can not get expression if forward last expression" `Quick (fun () ->
        let context = of_context "(a b)" in
        let actual =
          C.push_value context ~value:(T.Number "12")
          |> C.push_value ~value:(T.Number "13") |> C.push_value ~value:T.True |> C.current
        in
        Alcotest.(check @@ option exp) "result" actual None);
    Alcotest.test_case "do not indicate in nested expressions" `Quick (fun () ->
        let context = of_context "((a b) c)" |> C.push_value ~value:T.True in
        let actual = C.current context in
        let expected = Some (parse "c") in
        Alcotest.(check @@ option exp) "empty list" actual expected);
  ]

let tests = expression_test @ closure_test
