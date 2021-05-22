module P = Ocaml_scheme.Parser
module L = Ocaml_scheme.Lexer
module T = Ocaml_scheme.Type
module D = Ocaml_scheme.Data_type
module C = Ocaml_scheme.Eval_context
module Pr = Ocaml_scheme.Printer
module EP = Ocaml_scheme.Execution_pointer

module Status = struct
  type t = {
    value : int;
    mutable ep : EP.t;
  }

  let clone t = { value = t.value; ep = t.ep |> EP.clone }

  let execution_pointer t = t.ep

  let set_execution_pointer ep t = t.ep <- ep
end

let exp = Alcotest.testable Pr.pp ( = )

let next_state_t = Alcotest.testable C.next_state_pp ( = )

let parse str = Lexing.from_string str |> P.program L.token |> List.hd

let to_ep str = parse str |> EP.make

let to_list = Ocaml_scheme.Internal_lib.list_to_scheme_list

let tests =
  [
    Alcotest.test_case "get next instruction if exists" `Quick (fun () ->
        let context = C.make (module Status) { Status.ep = to_ep "(1 2)"; value = 1 } in
        let module I = (val context : C.Instance with type status = Status.t) in
        let actual = I.(next instance) in
        let expected = `Continue (parse "1") in
        Alcotest.(check @@ next_state_t) "result" actual expected);
    Alcotest.test_case "get end instruction marker if it has no expression rest" `Quick (fun () ->
        let context = C.make (module Status) { Status.ep = to_ep "(1)"; value = 1 } in
        let module I = (val context : C.Instance with type status = Status.t) in
        I.(next instance) |> ignore;
        let actual = I.(next instance) in
        let expected = `End_instruction in
        Alcotest.(check @@ next_state_t) "result" actual expected);
    Alcotest.test_case "get popped when pushed continuation is popped" `Quick (fun () ->
        let context = C.make (module Status) { Status.ep = to_ep "((+ 1))"; value = 1 } in
        let module I = (val context : C.Instance with type status = Status.t) in
        I.(next instance) |> ignore;
        I.(push_continuation instance { Status.ep = to_ep "(+ 1)"; value = 2 });
        I.(pop_continuation instance @@ parse "3");
        let actual = I.(next instance) in
        let expected = `Popped (parse "3") in
        Alcotest.(check @@ next_state_t) "result" actual expected);
    Alcotest.test_case "get finished when popped top of continuation" `Quick (fun () ->
        let context = C.make (module Status) { Status.ep = to_ep "((+ 1))"; value = 1 } in
        let module I = (val context : C.Instance with type status = Status.t) in
        I.(next instance) |> ignore;
        I.(push_continuation instance { Status.ep = to_ep "(+ 1)"; value = 2 });
        I.(pop_continuation instance @@ parse "3");
        I.(next instance) |> ignore;
        let actual =
          I.(next instance) |> function `Finished { Status.value = v; _ } -> v | _ -> Alcotest.fail "failed"
        in
        let expected = 1 in
        Alcotest.(check int) "result" actual expected);
  ]
