module G = Ocaml_scheme.Unique_symbol_generator

let tests =
  [
    Alcotest.test_case "get unique symbol sequence" `Quick (fun () ->
        let instance = G.make () in

        Alcotest.(check string) "first" ";0" (G.next instance);
        Alcotest.(check string) "second" ";1" (G.next instance));
    Alcotest.test_case "do not override sequence between generators" `Quick (fun () ->
        let instance1 = G.make () in
        let instance2 = G.make () in

        Alcotest.(check string) "first" ";0" (G.next instance1);
        Alcotest.(check string) "first" ";0" (G.next instance2));
  ]
