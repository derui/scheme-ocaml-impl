module P = Ocaml_scheme.Parser
module L = Ocaml_scheme.Lexer
module S = Ocaml_scheme.Syntax

let data = Alcotest.testable S.data_pp ( = )

let tests =
  [
    Alcotest.test_case "read empty list" `Quick (fun () ->
        let actual = Lexing.from_string "()" |> P.program L.token |> List.hd in
        Alcotest.(check @@ data) "empty list" actual S.Empty_list);
    Alcotest.test_case "read symbol" `Quick (fun () ->
        let actual = Lexing.from_string "abcde->efg" |> P.program L.token |> List.hd in
        Alcotest.(check @@ data) "symbol" actual (S.Symbol "abcde->efg"));
    Alcotest.test_case "read number" `Quick (fun () ->
        let actual = Lexing.from_string "13151341" |> P.program L.token |> List.hd in
        Alcotest.(check @@ data) "symbol" actual (S.Number "13151341"));
    Alcotest.test_case "read cons" `Quick (fun () ->
        let actual = Lexing.from_string "(1)" |> P.program L.token |> List.hd in
        let expected = S.Cons (S.Number "1", S.Empty_list) in
        Alcotest.(check @@ data) "symbol" actual expected);
    Alcotest.test_case "read false" `Quick (fun () ->
        let actual = Lexing.from_string "#f" |> P.program L.token |> List.hd in
        let expected = S.False in
        Alcotest.(check @@ data) "false" actual expected);
    Alcotest.test_case "read true" `Quick (fun () ->
        let actual = Lexing.from_string "#t" |> P.program L.token |> List.hd in
        let expected = S.True in
        Alcotest.(check @@ data) "true" actual expected);
    Alcotest.test_case "read list as cons chain" `Quick (fun () ->
        let actual = Lexing.from_string "(1 b-1 #t)" |> P.program L.token |> List.hd in
        let expected = S.Cons (S.Number "1", S.Cons (S.Symbol "b-1", S.Cons (S.True, S.Empty_list))) in
        Alcotest.(check @@ data) "symbol" actual expected);
    Alcotest.test_case "read nest list" `Quick (fun () ->
        let actual = Lexing.from_string "(1 (a 2))" |> P.program L.token |> List.hd in
        let expected =
          S.Cons (S.Number "1", S.Cons (S.Cons (S.Symbol "a", S.Cons (S.Number "2", S.Empty_list)), S.Empty_list))
        in
        Alcotest.(check @@ data) "symbol" actual expected);
  ]
