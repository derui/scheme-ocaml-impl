module P = Ocaml_scheme.Parser
module Pr = Ocaml_scheme.Printer
module L = Ocaml_scheme.Lexer
module T = Ocaml_scheme.Type
module D = Ocaml_scheme.Data_type

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
        let expected = T.cons (T.Number "1") T.Empty_list in
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
        let expected = T.cons (T.Number "1") @@ T.cons (T.Symbol "b-1") @@ T.cons T.True T.Empty_list in
        Alcotest.(check @@ data) "symbol" actual expected);
    Alcotest.test_case "read nest list" `Quick (fun () ->
        let actual = Lexing.from_string "(1 (a 2))" |> P.program L.token |> List.hd in
        let expected =
          T.(cons (T.Number "1") @@ cons (cons (T.Symbol "a") @@ cons (T.Number "2") T.Empty_list) T.Empty_list)
        in
        Alcotest.(check @@ data) "symbol" actual expected);
    Alcotest.test_case "read quasiquote" `Quick (fun () ->
        let actual = Lexing.from_string "`(1 (a 2))" |> P.program L.token |> List.hd in
        let expected =
          T.(
            cons (Symbol "quasiquote")
            @@ cons
                 (cons (Number "1") @@ cons (cons (T.Symbol "a") @@ cons (T.Number "2") T.Empty_list) T.Empty_list)
                 Empty_list)
        in
        Alcotest.(check @@ data) "quasiquote" actual expected);
    Alcotest.test_case "read quasiquote: empty list" `Quick (fun () ->
        let actual = Lexing.from_string "`()" |> P.program L.token |> List.hd in
        let expected = T.(cons (T.Symbol "quasiquote") @@ cons Empty_list Empty_list) in
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
        let expected = T.(cons (Symbol "a") (Number "2")) in
        Alcotest.(check @@ data) "unquote" actual expected);
    Alcotest.test_case "read nested dotted list" `Quick (fun () ->
        let actual = Lexing.from_string "((a . 1) . 2)" |> P.program L.token |> List.hd in
        let expected = T.(cons (cons (Symbol "a") (T.Number "1")) (T.Number "2")) in
        Alcotest.(check @@ data) "unquote" actual expected);
    Alcotest.test_case "read multiple list as program" `Quick (fun () ->
        let actual = Lexing.from_string "(a 1) \n(b 2)" |> P.program L.token in
        let expected =
          [
            T.(cons (Symbol "a") @@ cons (Number "1") Empty_list); T.(cons (Symbol "b") @@ cons (Number "2") Empty_list);
          ]
        in
        Alcotest.(check @@ list data) "program" actual expected);
    Alcotest.test_case "ignore line comment" `Quick (fun () ->
        let actual = Lexing.from_string ";;(a 1) \n(b 2)" |> P.program L.token in
        let expected = [ T.(cons (Symbol "b") @@ cons (Number "2") Empty_list) ] in
        Alcotest.(check @@ list data) "program" actual expected);
    Alcotest.test_case "simple string" `Quick (fun () ->
        let actual = Lexing.from_string "\"abcdef\\a\"" |> P.program L.token in
        let expected =
          [
            T.Scheme_string
              [
                D.Scheme_char.Char "a";
                D.Scheme_char.Char "b";
                D.Scheme_char.Char "c";
                D.Scheme_char.Char "d";
                D.Scheme_char.Char "e";
                D.Scheme_char.Char "f";
                D.Scheme_char.alarm;
              ];
          ]
        in
        Alcotest.(check @@ list data) "program" actual expected);
  ]
