module S = Ocaml_scheme.Syntax
module P = Ocaml_scheme.Printer

let tests =
  [
    Alcotest.test_case "print symbol" `Quick (fun () ->
        let actual = S.Symbol "foobar-f" |> P.print in
        let expected = "foobar-f" in
        Alcotest.(check string) "symbol" actual expected);
    Alcotest.test_case "print number" `Quick (fun () ->
        let actual = S.Number "105" |> P.print in
        let expected = "105" in
        Alcotest.(check string) "number" actual expected);
    Alcotest.test_case "print empty list" `Quick (fun () ->
        let actual = S.Empty_list |> P.print in
        let expected = "()" in
        Alcotest.(check string) "empty list" actual expected);
    Alcotest.test_case "print list" `Quick (fun () ->
        let actual = S.Cons (S.Symbol "a", S.Empty_list) |> P.print in
        let expected = "(a)" in
        Alcotest.(check string) "list" actual expected);
    Alcotest.test_case "print nested list" `Quick (fun () ->
        let actual = S.Cons (S.Symbol "a", S.Cons (S.Cons (S.False, S.Empty_list), S.Empty_list)) |> P.print in
        let expected = "(a (#f))" in
        Alcotest.(check string) "list" actual expected);
    Alcotest.test_case "print dotted list" `Quick (fun () ->
        let actual = S.Cons (S.Symbol "a", S.Cons (S.Cons (S.False, S.True), S.Number "15")) |> P.print in
        let expected = "(a (#f . #t) . 15)" in
        Alcotest.(check string) "list" actual expected);
  ]
