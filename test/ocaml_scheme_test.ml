let tests = [ ("Environment", Environment_test.tests); ("Parser", Parser_test.tests) ]

let () = Alcotest.run "OCaml scheme learning" tests
