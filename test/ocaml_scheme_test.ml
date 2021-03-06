let tests =
  [
    ("Environment", Environment_test.tests);
    ("Special form", Special_form_test.tests);
    ("Parser", Parser_test.tests);
    ("Eval", Eval_test.tests);
    ("Printer", Printer_test.tests);
    ("Syntax rule library", Syntax_rule_lib_test.tests);
    ("Syntax rule pattern matcher", Syntax_rule_pattern_matcher_test.tests);
    ("Syntax rule template expansion", Syntax_rule_template_test.tests);
    ("Unique symbol generator", Unique_symbol_generator_test.tests);
    ("Evaluation stack", Eval_stack_test.tests);
    ("Evaluation context", Eval_context_test.tests);
    ("Execution pointer", Execution_pointer_test.tests);
    ("Primitive operations", Primitive_op_test.tests);
    ("Library", Library_test.tests);
    ("List parser", List_parser_test.tests);
    ("Import", Import_test.tests);
    ("Import parser", Import_parser_test.tests);
    ("Library parser", Library_parser_test.tests);
    ("Runtime", Runtime_test.tests);
    ("cond-expand parser", Cond_expand_parser_test.tests);
  ]

let () = Alcotest.run "OCaml scheme learning" tests
