module P = Ocaml_scheme.Parser
module Pr = Ocaml_scheme.Printer
module L = Ocaml_scheme.Lexer
module T = Ocaml_scheme.Type

let data = Alcotest.testable Pr.pp ( = )

let tests =
  [
    Alcotest.test_case "read empty list" `Quick (fun () ->
        let actual = Lexing.from_string "()" |> P.program L.token |> List.hd in
        Alcotest.(check @@ data) "empty list" actual T.Empty_list);
    Alcotest.test_case "read symbol" `Quick (fun () ->
        let actual = Lexing.from_string "abcde->efg" |> P.program L.token |> List.hd in
        Alcotest.(check @@ data) "symbol" actual (T.Symbol "abcde->efg"));
    Alcotest.test_case "read number" `Quick (fun () ->
        let actual = Lexing.from_string "13151341" |> P.program L.token |> List.hd in
        Alcotest.(check @@ data) "symbol" actual (T.Number "13151341"));
    Alcotest.test_case "read cons" `Quick (fun () ->
        let actual = Lexing.from_string "(1)" |> P.program L.token |> List.hd in
        let expected = T.Cons (T.Number "1", T.Empty_list) in
        Alcotest.(check @@ data) "symbol" actual expected);
    Alcotest.test_case "read false" `Quick (fun () ->
        let actual = Lexing.from_string "#f" |> P.program L.token |> List.hd in
        let expected = T.False in
        Alcotest.(check @@ data) "false" actual expected);
    Alcotest.test_case "read true" `Quick (fun () ->
        let actual = Lexing.from_string "#t" |> P.program L.token |> List.hd in
        let expected = T.True in
        Alcotest.(check @@ data) "true" actual expected);
    Alcotest.test_case "read list as cons chain" `Quick (fun () ->
        let actual = Lexing.from_string "(1 b-1 #t)" |> P.program L.token |> List.hd in
        let expected = T.Cons (T.Number "1", T.Cons (T.Symbol "b-1", T.Cons (T.True, T.Empty_list))) in
        Alcotest.(check @@ data) "symbol" actual expected);
    Alcotest.test_case "read nest list" `Quick (fun () ->
        let actual = Lexing.from_string "(1 (a 2))" |> P.program L.token |> List.hd in
        let expected =
          T.Cons (T.Number "1", T.Cons (T.Cons (T.Symbol "a", T.Cons (T.Number "2", T.Empty_list)), T.Empty_list))
        in
        Alcotest.(check @@ data) "symbol" actual expected);
    Alcotest.test_case "read quasiquote" `Quick (fun () ->
        let actual = Lexing.from_string "`(1 (a 2))" |> P.program L.token |> List.hd in
        let expected =
          T.Cons
            ( T.Symbol "quasiquote",
              T.Cons
                ( T.Cons
                    (T.Number "1", T.Cons (T.Cons (T.Symbol "a", T.Cons (T.Number "2", T.Empty_list)), T.Empty_list)),
                  T.Empty_list ) )
        in
        Alcotest.(check @@ data) "quasiquote" actual expected);
    Alcotest.test_case "read quasiquote: empty list" `Quick (fun () ->
        let actual = Lexing.from_string "`()" |> P.program L.token |> List.hd in
        let expected = T.Cons (T.Symbol "quasiquote", T.Cons (T.Empty_list, T.Empty_list)) in
        Alcotest.(check @@ data) "quasiquote" actual expected);
    Alcotest.test_case "read unquote" `Quick (fun () ->
        let actual = Lexing.from_string "`((,a 2))" |> P.program L.token |> List.hd in
        let expected = Lexing.from_string "(quasiquote (((unquote a) 2)))" |> P.program L.token |> List.hd in
        Alcotest.(check @@ data) "unquote" actual expected);
    Alcotest.test_case "read quote" `Quick (fun () ->
        let actual = Lexing.from_string "'((a 2))" |> P.program L.token |> List.hd in
        let expected = Lexing.from_string "(quote ((a 2)))" |> P.program L.token |> List.hd in
        Alcotest.(check @@ data) "unquote" actual expected);
    Alcotest.test_case "read dotted list" `Quick (fun () ->
        let actual = Lexing.from_string "(a . 2)" |> P.program L.token |> List.hd in
        let expected = T.Cons (T.Symbol "a", T.Number "2") in
        Alcotest.(check @@ data) "unquote" actual expected);
    Alcotest.test_case "read nested dotted list" `Quick (fun () ->
        let actual = Lexing.from_string "((a . 1) . 2)" |> P.program L.token |> List.hd in
        let expected = T.Cons (T.Cons (Symbol "a", T.Number "1"), T.Number "2") in
        Alcotest.(check @@ data) "unquote" actual expected);
  ]
