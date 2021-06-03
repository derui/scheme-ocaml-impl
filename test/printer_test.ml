module T = Ocaml_scheme.Type
module P = Ocaml_scheme.Printer

let tests =
  [
    Alcotest.test_case "print symbol" `Quick (fun () ->
        let actual = T.Symbol "foobar-f" |> P.print in
        let expected = "foobar-f" in
        Alcotest.(check string) "symbol" actual expected);
    Alcotest.test_case "print number" `Quick (fun () ->
        let actual = T.Number "105" |> P.print in
        let expected = "105" in
        Alcotest.(check string) "number" actual expected);
    Alcotest.test_case "print empty list" `Quick (fun () ->
        let actual = T.Empty_list |> P.print in
        let expected = "()" in
        Alcotest.(check string) "empty list" actual expected);
    Alcotest.test_case "print list" `Quick (fun () ->
        let actual = T.(cons (T.Symbol "a") T.Empty_list) |> P.print in
        let expected = "(a)" in
        Alcotest.(check string) "list" actual expected);
    Alcotest.test_case "print nested list" `Quick (fun () ->
        let actual = T.(cons (Symbol "a") @@ cons (cons False Empty_list) Empty_list) |> P.print in
        let expected = "(a (#f))" in
        Alcotest.(check string) "list" actual expected);
    Alcotest.test_case "print dotted list" `Quick (fun () ->
        let actual = T.(cons (Symbol "a") @@ cons (cons False T.True) (T.Number "15")) |> P.print in
        let expected = "(a (#f . #t) . 15)" in
        Alcotest.(check string) "list" actual expected);
  ]
