module P = Ocaml_scheme.Parser
module Pr = Ocaml_scheme.Printer
module L = Ocaml_scheme.Lexer
module T = Ocaml_scheme.Type
module F = Ocaml_scheme.Special_form
module E = Ocaml_scheme.Environment

let data = Alcotest.testable Pr.pp ( = )

let env ?(bindings = []) () =
  Ocaml_scheme.Environment.make ([ ("+", T.Primitive_fun Ocaml_scheme.Primitive_op.Number_op.Export.plus) ] @ bindings)

let parse_exp v = Lexing.from_string v |> P.program L.token |> List.hd

let to_scheme_list list = List.rev list |> List.fold_left (fun accum v -> T.Cons (v, accum)) T.Empty_list

let tests =
  let error_t = Alcotest.of_pp Fmt.nop in
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
  ]
