module P = Ocaml_scheme.Parser
module L = Ocaml_scheme.Lexer
module E = Ocaml_scheme.Type.External_representation
module T = Ocaml_scheme.Type
module D = Ocaml_scheme.Data_type
module C = Ocaml_scheme.Eval_context

let exp = Alcotest.testable E.pp ( = )

let parse str = Lexing.from_string str |> P.program L.token |> List.hd

let to_list = Ocaml_scheme.Internal_lib.External_representation.list_to_scheme_list

let tests =
  let of_context source = parse source |> C.make in
  [
    Alcotest.test_case "expression of context" `Quick (fun () ->
        let context = of_context "(a b)" in
        let actual = C.whole_expression context in
        let expected = to_list [ E.symbol "a"; E.symbol "b" ] in
        Alcotest.(check @@ exp) "exp" actual expected);
    Alcotest.test_case "get current expression to evaluate in evaluation context" `Quick (fun () ->
        let context = of_context "(a b)" in
        let actual = C.current context in
        let expected = E.symbol "a" |> Option.some in
        Alcotest.(check @@ option exp) "empty list" actual expected);
    Alcotest.test_case "store evaluated value" `Quick (fun () ->
        let context = of_context "(a b)" in
        let actual = C.store_value context ~value:(T.Number "12") |> C.evaluated_values in
        let expected = [ T.Number "12" ] in
        Alcotest.(check @@ list @@ of_pp Fmt.nop) "result" actual expected);
    Alcotest.test_case "forward indicator when store evaluated value" `Quick (fun () ->
        let context = of_context "(a b)" in
        let actual = C.store_value context ~value:(T.Number "12") |> C.current in
        let expected = Some (E.symbol "b") in
        Alcotest.(check @@ option exp) "result" actual expected);
    Alcotest.test_case "can not get expression if forward last expression" `Quick (fun () ->
        let context = of_context "(a b)" in
        let actual =
          C.store_value context ~value:(T.Number "12")
          |> C.store_value ~value:(T.Number "13") |> C.store_value ~value:T.True |> C.current
        in
        Alcotest.(check @@ option exp) "result" actual None);
    Alcotest.test_case "do not indicate in nested expressions" `Quick (fun () ->
        let context = of_context "((a b) c)" |> C.store_value ~value:T.True in
        let actual = C.current context in
        let expected = Some (parse "c") in
        Alcotest.(check @@ option exp) "empty list" actual expected);
    Alcotest.test_case "expand expression current indicated" `Quick (fun () ->
        let context = of_context "(a b)" in
        let actual = C.expand context ~expr:(parse "(c d)") |> C.current in
        let expected = Some (parse "(c d)") in
        Alcotest.(check @@ option exp) "empty list" actual expected);
    Alcotest.test_case "expand should be mutable operation" `Quick (fun () ->
        let context = of_context "(a b)" in
        C.expand context ~expr:(parse "(c d)") |> ignore;
        let actual = C.current context in
        let expected = Some (parse "(c d)") in
        Alcotest.(check @@ option exp) "empty list" actual expected);
  ]
