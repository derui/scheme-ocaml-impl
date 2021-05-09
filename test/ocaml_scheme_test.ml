let tests =
  [
    ("Environment", Environment_test.tests);
    (* ("Special form", Special_form_test.tests); *)
    ("Parser", Parser_test.tests);
    ("Eval", Eval_test.tests);
    ("Printer", Printer_test.tests);
    ("Syntax rule library", Syntax_rule_lib_test.tests);
    ("Syntax rule pattern matcher", Syntax_rule_pattern_matcher_test.tests);
    ("Syntax rule template expansion", Syntax_rule_template_test.tests);
    ("Unique symbol generator", Unique_symbol_generator_test.tests);
    ("Evaluation stack", Eval_stack_test.tests);
  ]

let () = Alcotest.run "OCaml scheme learning" tests
