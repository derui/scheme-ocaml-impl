module E = Ocaml_scheme.Environment
module S = Ocaml_scheme.Syntax

let tests =
  [
    Alcotest.test_case "should create empty environment" `Quick (fun () ->
        let env = E.make [] in
        let actual = E.get env ~key:"key" in
        let expected = Error "unbound variable key" in
        Alcotest.(check @@ result (of_pp Fmt.nop) string) "empty" actual expected);
    Alcotest.test_case "should get value bounded in the environment" `Quick (fun () ->
        let value = E.Value (S.Number "10") in
        let env = E.make [ ("key", value) ] in
        let actual = E.get env ~key:"key" in
        let expected = Ok value in
        Alcotest.(check @@ result (of_pp Fmt.nop) string) "get value" actual expected);
    Alcotest.test_case "should set value bounded in the environment" `Quick (fun () ->
        let value = E.Value (S.Number "10") in
        let new_value = E.Value (S.Symbol "sym") in
        let env = E.make [ ("key", value) ] in
        E.set env ~key:"key" ~v:new_value |> ignore;
        let actual = E.get env ~key:"key" in
        let expected = Ok new_value in
        Alcotest.(check @@ result (of_pp Fmt.nop) string) "set value" actual expected);
    Alcotest.test_case "should resolve value from parent if not found from current environment" `Quick (fun () ->
        let value = E.Value (S.Number "10") in
        let parent_env = E.make [ ("key", value) ] in
        let env = E.make ~parent_env [] in
        let actual = E.get env ~key:"key" in
        let expected = Ok value in
        Alcotest.(check @@ result (of_pp Fmt.nop) string) "get from parent" actual expected);
    Alcotest.test_case "should set value in parent environment if not found from current environment" `Quick (fun () ->
        let value = E.Value (S.Number "10") in
        let new_value = E.Value (S.Symbol "sym") in
        let parent_env = E.make [ ("key", value) ] in
        let env = E.make ~parent_env [] in
        E.set env ~key:"key" ~v:new_value |> ignore;
        let actual = E.get parent_env ~key:"key" in
        let expected = Ok new_value in
        Alcotest.(check @@ result (of_pp Fmt.nop) string) "set value in parent" actual expected);
    Alcotest.test_case "should shadow value bounded by same symbol in parent and current environments" `Quick (fun () ->
        let value = E.Value (S.Number "10") in
        let value_current = E.Value (S.Number "11") in
        let parent_env = E.make [ ("key", value) ] in
        let env = E.make ~parent_env [ ("key", value_current) ] in
        let actual = E.get env ~key:"key" in
        let expected = Ok value_current in
        Alcotest.(check @@ result (of_pp Fmt.nop) string) "get " actual expected);
    Alcotest.test_case "should set value in parent environment if not found from current environment" `Quick (fun () ->
        let value = E.Value (S.Number "10") in
        let new_value = E.Value (S.Symbol "sym") in
        let parent_env = E.make [ ("key", value) ] in
        let env = E.make ~parent_env [ ("key", value) ] in
        E.set env ~key:"key" ~v:new_value |> ignore;
        let in_parent = E.get parent_env ~key:"key" and in_current = E.get env ~key:"key" in
        Alcotest.(check @@ result (of_pp Fmt.nop) string) "in parent" in_parent (Ok value);
        Alcotest.(check @@ result (of_pp Fmt.nop) string) "in current" in_current (Ok new_value));
  ]
