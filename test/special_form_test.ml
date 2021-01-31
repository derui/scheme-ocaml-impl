module P = Ocaml_scheme.Parser
module Pr = Ocaml_scheme.Printer
module L = Ocaml_scheme.Lexer
module T = Ocaml_scheme.Type
module F = Ocaml_scheme.Special_form

let data = Alcotest.testable Pr.pp ( = )

let env =
  Ocaml_scheme.Environment.make [ ("+", T.Value (T.Primitive_fun Ocaml_scheme.Primitive_op.Number_op.Export.plus)) ]

let parse_exp v = Lexing.from_string v |> P.program L.token |> List.hd

let to_scheme_list list = List.rev list |> List.fold_left (fun accum v -> T.Cons (v, accum)) T.Empty_list

let tests =
  [
    Alcotest.test_case "quasiquote: quote empty list" `Quick (fun () ->
        let actual = parse_exp "(())" |> F.eval_quasiquote env in
        Alcotest.(check @@ result data string) "empty list" actual (Ok T.Empty_list));
    Alcotest.test_case "quasiquote: quote list " `Quick (fun () ->
        let actual = parse_exp "((1 2))" |> F.eval_quasiquote env in
        Alcotest.(check @@ result data string) "empty list" actual (Ok (to_scheme_list [ T.Number "1"; T.Number "2" ])));
    Alcotest.test_case "quasiquote: quote symbol " `Quick (fun () ->
        let actual = parse_exp "(a)" |> F.eval_quasiquote env in
        Alcotest.(check @@ result data string) "quasiquote" actual (Ok (T.Symbol "a")));
    Alcotest.test_case "unquote: eval number " `Quick (fun () ->
        let actual = parse_exp "(12)" |> F.eval_unquote env in
        Alcotest.(check @@ result data string) "unquote" actual (Ok (T.Number "12")));
    Alcotest.test_case "unquote: eval list" `Quick (fun () ->
        let actual = parse_exp "((+ 1 2))" |> F.eval_unquote env in
        Alcotest.(check @@ result data string) "unquote" actual (Ok (T.Number "3")));
    Alcotest.test_case "unquote: eval boolean" `Quick (fun () ->
        let actual = parse_exp "(#f)" |> F.eval_unquote env in
        Alcotest.(check @@ result data string) "unquote" actual (Ok T.False));
    Alcotest.test_case "quote: quote all" `Quick (fun () ->
        let actual = parse_exp "((+ 1 2))" |> F.eval_quote env in
        Alcotest.(check @@ result data string)
          "unquote" actual
          (Ok (to_scheme_list [ T.Symbol "+"; T.Number "1"; T.Number "2" ])));
  ]
