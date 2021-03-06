module E = Ocaml_scheme.Environment
module T = Ocaml_scheme.Type
module Pr = Ocaml_scheme.Printer

let data = Alcotest.testable Pr.pp ( = )

let tests =
  [
    Alcotest.test_case "should create empty environment" `Quick (fun () ->
        let env = E.make [] in
        let actual = E.get env ~key:"key" in
        let expected = None in
        Alcotest.(check @@ option (of_pp Fmt.nop)) "empty" actual expected);
    Alcotest.test_case "should get value bounded in the environment" `Quick (fun () ->
        let value = T.Number "10" in
        let env = E.make [ ("key", value) ] in
        let actual = E.get env ~key:"key" in
        let expected = Some value in
        Alcotest.(check @@ option (of_pp Fmt.nop)) "get value" actual expected);
    Alcotest.test_case "should set value bounded in the environment" `Quick (fun () ->
        let value = T.Number "10" in
        let new_value = T.Symbol "sym" in
        let env = E.make [ ("key", value) ] in
        E.set env ~key:"key" ~v:new_value;
        let actual = E.get env ~key:"key" in
        let expected = Some new_value in
        Alcotest.(check @@ option (of_pp Fmt.nop)) "set value" actual expected);
    Alcotest.test_case "should resolve value from parent if not found from current environment" `Quick (fun () ->
        let value = T.Number "10" in
        let parent_env = E.make [ ("key", value) ] in
        let env = E.make ~parent_env [] in
        let actual = E.get env ~key:"key" in
        let expected = Some value in
        Alcotest.(check @@ option (of_pp Fmt.nop)) "get from parent" actual expected);
    Alcotest.test_case "should set value in parent environment if not found from current environment" `Quick (fun () ->
        let value = T.Number "10" in
        let new_value = T.Symbol "sym" in
        let parent_env = E.make [ ("key", value) ] in
        let env = E.make ~parent_env [] in
        E.replace env ~key:"key" ~v:new_value |> ignore;
        let actual = E.get parent_env ~key:"key" in
        let expected = Some new_value in
        Alcotest.(check @@ option data) "set value in parent" actual expected);
    Alcotest.test_case "should shadow value bounded by same symbol in parent and current environments" `Quick (fun () ->
        let value = T.Number "10" in
        let value_current = T.Number "11" in
        let parent_env = E.make [ ("key", value) ] in
        let env = E.make ~parent_env [ ("key", value_current) ] in
        let actual = E.get env ~key:"key" in
        let expected = Some value_current in
        Alcotest.(check @@ option (of_pp Fmt.nop)) "get " actual expected);
    Alcotest.test_case "should set value in parent environment if not found from current environment" `Quick (fun () ->
        let value = T.Number "10" in
        let new_value = T.Symbol "sym" in
        let parent_env = E.make [ ("key", value) ] in
        let env = E.make ~parent_env [ ("key", value) ] in
        E.set env ~key:"key" ~v:new_value |> ignore;
        let in_parent = E.get parent_env ~key:"key" and in_current = E.get env ~key:"key" in
        Alcotest.(check @@ option (of_pp Fmt.nop)) "in parent" in_parent (Some value);
        Alcotest.(check @@ option (of_pp Fmt.nop)) "in current" in_current (Some new_value));
    Alcotest.test_case "should return error if symbol is not defined" `Quick (fun () ->
        let value = T.Number "10" in
        let env = E.make [] in
        E.replace env ~key:"key" ~v:value |> ignore;
        let actual = E.get env ~key:"key" in
        Alcotest.(check @@ option data) "in current" actual None);
  ]
