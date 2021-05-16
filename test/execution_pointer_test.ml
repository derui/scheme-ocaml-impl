module P = Ocaml_scheme.Parser
module L = Ocaml_scheme.Lexer
module T = Ocaml_scheme.Type
module D = Ocaml_scheme.Data_type
module EP = Ocaml_scheme.Execution_pointer
module Pr = Ocaml_scheme.Printer

let exp = Alcotest.testable Pr.pp ( = )

let parse str = Lexing.from_string str |> P.program L.token |> List.hd

let to_list = Ocaml_scheme.Internal_lib.list_to_scheme_list

let tests =
  let of_context source = parse source |> EP.make in
  [
    Alcotest.test_case "get current expression of current pointed" `Quick (fun () ->
        let context = of_context "(a b)" in
        let actual = EP.current context in
        let expected = Some (T.Symbol "a") in
        Alcotest.(check @@ option exp) "exp" actual expected);
    Alcotest.test_case "move pointer to next" `Quick (fun () ->
        let context = of_context "(a b)" in
        let context = EP.next context in
        let actual = EP.current context in
        let expected = T.Symbol "b" |> Option.some in
        Alcotest.(check @@ option exp) "empty list" actual expected);
    Alcotest.test_case "assert failed if expression is not list" `Quick (fun () ->
        try
          of_context "a" |> ignore;
          Alcotest.fail "failed"
        with Assert_failure _ -> ());
    Alcotest.test_case "return None if it points at last" `Quick (fun () ->
        let context = of_context "(a b)" in
        let context = EP.next context in
        let context = EP.next context in
        let actual = EP.current context in
        let expected = None in
        Alcotest.(check @@ option exp) "result" actual expected);
    Alcotest.test_case "get dot-pair " `Quick (fun () ->
        let context = of_context "(a . b)" in
        let context = EP.next context in
        let actual = EP.current context in
        let expected = Some (T.Symbol "b") in
        Alcotest.(check @@ option exp) "result" actual expected);
  ]
