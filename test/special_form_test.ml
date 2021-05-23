module P = Ocaml_scheme.Parser
module Pr = Ocaml_scheme.Printer
module L = Ocaml_scheme.Lexer
module T = Ocaml_scheme.Type
module F = Ocaml_scheme.Special_form
module E = Ocaml_scheme.Environment
module D = Ocaml_scheme.Data_type

let data = Alcotest.testable Pr.pp ( = )

let env ?(bindings = []) () =
  Ocaml_scheme.Environment.make ([ ("+", T.Primitive_fun Ocaml_scheme.Primitive_op.Number_op.Export.plus) ] @ bindings)

let parse_exp v = Lexing.from_string v |> P.program L.token |> List.hd

let to_scheme_list list = List.rev list |> List.fold_left (fun accum v -> T.Cons (v, accum)) T.Empty_list

let tests =
  let error_t = Alcotest.of_pp T.scheme_error_pp in
  [
    Alcotest.test_case "if: return second value if false" `Quick (fun () ->
        let env = env () in
        let actual = parse_exp "(#f 1 2)" |> F.eval_if env in
        Alcotest.(check @@ result data error_t) "false" actual (Ok (T.Number "2")));
    Alcotest.test_case "if: return first value if true" `Quick (fun () ->
        let env = env () in
        let actual = parse_exp "(#t 1 2)" |> F.eval_if env in
        Alcotest.(check @@ result data error_t) "false" actual (Ok (T.Number "1")));
    Alcotest.test_case "define: define value at symbol" `Quick (fun () ->
        let env = env () in
        let actual = parse_exp "(v 1)" |> F.eval_define env in
        Alcotest.(check @@ result data error_t) "false" actual (Ok (T.Number "1"));
        let v = E.get env ~key:"v" in
        Alcotest.(check @@ option data) "false" v (Some (T.Number "1")));
    Alcotest.test_case "set-force: set value at symbol" `Quick (fun () ->
        let env = env ~bindings:[ ("v", T.Number "1") ] () in
        let actual = parse_exp "(v 3)" |> F.eval_set_force env in
        Alcotest.(check @@ result data error_t) "false" actual (Ok (T.Number "3"));
        let v = E.get env ~key:"v" in
        Alcotest.(check @@ option data) "false" v (Some (T.Number "3")));
    Alcotest.test_case "quote: return value as input" `Quick (fun () ->
        let env = env () in
        let actual = parse_exp "(v)" |> F.eval_quote env in
        Alcotest.(check @@ result data error_t) "value" actual (Ok (T.Symbol "v")));
    Alcotest.test_case "quote: raise error if argument is invalid" `Quick (fun () ->
        let env = env () in
        let actual = parse_exp "()" |> F.eval_quote env in
        Alcotest.(check @@ result data error_t) "empty list" actual (T.raise_syntax_error "malformed quote: (quote)");
        let actual = parse_exp "(1 2)" |> F.eval_quote env in
        Alcotest.(check @@ result data error_t)
          "more arguments" actual
          (T.raise_syntax_error "malformed quote: (quote 1 2)"));
    Alcotest.test_case "lambda: get closure" `Quick (fun () ->
        let env = env () in
        let actual = parse_exp "((a) (+ a 2))" |> F.eval_lambda env in
        let actual =
          match actual with
          | Ok (T.Closure { argument_formal; body; _ }) -> (argument_formal, body)
          | _ -> Alcotest.fail "invalid route"
        in
        Alcotest.(check @@ pair (of_pp Fmt.nop) data)
          "value" actual
          (D.Argument_formal.Fixed [ "a" ], parse_exp "((+ a 2))"));
  ]
