let tests =
  [
    ("Environment", Environment_test.tests);
    ("Special form", Special_form_test.tests);
    ("Parser", Parser_test.tests);
    ("Eval", Eval_test.tests);
    ("Printer", Printer_test.tests);
  ]

let () = Alcotest.run "OCaml scheme learning" tests
