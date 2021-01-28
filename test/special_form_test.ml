module P = Ocaml_scheme.Parser
module L = Ocaml_scheme.Lexer
module S = Ocaml_scheme.Syntax
module F = Ocaml_scheme.Special_form

let data = Alcotest.testable S.data_pp ( = )

let env =
  Ocaml_scheme.Environment.make [ ("+", S.Value (S.Primitive_fun Ocaml_scheme.Primitive_op.Number_op.Export.plus)) ]

let parse_exp v = Lexing.from_string v |> P.program L.token |> List.hd

let to_scheme_list list = List.rev list |> List.fold_left (fun accum v -> S.Cons (v, accum)) S.Empty_list

let tests =
  [
    Alcotest.test_case "quasiquote: quote empty list" `Quick (fun () ->
        let actual = parse_exp "(())" |> F.eval_quasiquote env in
        Alcotest.(check @@ result data string) "empty list" actual (Ok S.Empty_list));
    Alcotest.test_case "quasiquote: quote list " `Quick (fun () ->
        let actual = parse_exp "((1 2))" |> F.eval_quasiquote env in
        Alcotest.(check @@ result data string) "empty list" actual (Ok (to_scheme_list [ S.Number "1"; S.Number "2" ])));
    Alcotest.test_case "quasiquote: quote symbol " `Quick (fun () ->
        let actual = parse_exp "(a)" |> F.eval_quasiquote env in
        Alcotest.(check @@ result data string) "quasiquote" actual (Ok (S.Symbol "a")));
    Alcotest.test_case "unquote: eval number " `Quick (fun () ->
        let actual = parse_exp "(12)" |> F.eval_unquote env in
        Alcotest.(check @@ result data string) "unquote" actual (Ok (S.Number "12")));
    Alcotest.test_case "unquote: eval list" `Quick (fun () ->
        let actual = parse_exp "((+ 1 2))" |> F.eval_unquote env in
        Alcotest.(check @@ result data string) "unquote" actual (Ok (S.Number "3")));
    Alcotest.test_case "unquote: eval boolean" `Quick (fun () ->
        let actual = parse_exp "(#f)" |> F.eval_unquote env in
        Alcotest.(check @@ result data string) "unquote" actual (Ok S.False));
  ]
