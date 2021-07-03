module P = Ocaml_scheme.Cond_expand_parser
module CE = Ocaml_scheme.Cond_expand
module I = Ocaml_scheme.Import
module T = Ocaml_scheme.Type
module Pr = Ocaml_scheme.Printer
module F = Ocaml_scheme.Feature_query.Feature_requirement
module FI = Ocaml_scheme.Feature_query.Feature_identifier

let exp_pp = Alcotest.testable Pr.pp ( = )

let error_t = Alcotest.testable T.Scheme_error.pp ( = )

let expand_t = Alcotest.testable CE.pp ( = )

let parse v = Lexing.from_string v |> Ocaml_scheme.Parser.program Ocaml_scheme.Lexer.token |> List.hd

let list_to_scheme_list v = List.rev v |> List.fold_left (fun accum v -> T.cons v accum) T.Empty_list

let tests =
  [
    Alcotest.test_case "cond-expand parser: parse empty" `Quick (fun () ->
        let v = parse "(cond-expand)" in
        let actual = P.parse v in
        let expected = Ok { CE.clauses = []; else_expression = None } in
        Alcotest.(check @@ result expand_t error_t) "simple" expected actual);
    Alcotest.test_case "cond-expand parser: parse identity requirement clause" `Quick (fun () ->
        let v = parse "(cond-expand (r7rs 5))" in
        let actual = P.parse v in
        let expected =
          Ok
            {
              CE.clauses =
                [ { CE.Cond_expand_clause.feature_requirement = F.feature_identifier FI.R7RS; expression = parse "5" } ];
              else_expression = None;
            }
        in
        Alcotest.(check @@ result expand_t error_t) "simple" expected actual);
    Alcotest.test_case "cond-expand parser: parse library name requirement clause" `Quick (fun () ->
        let v = parse "(cond-expand ((library (a b c)) 5))" in
        let actual = P.parse v in
        let expected =
          Ok
            {
              CE.clauses =
                [ { CE.Cond_expand_clause.feature_requirement = F.library [ "a"; "b"; "c" ]; expression = parse "5" } ];
              else_expression = None;
            }
        in
        Alcotest.(check @@ result expand_t error_t) "simple" expected actual);
    Alcotest.test_case "cond-expand parser: parse and clause" `Quick (fun () ->
        let v = parse "(cond-expand ((and r7rs (library (a b c))) 5))" in
        let actual = P.parse v in
        let expected =
          Ok
            {
              CE.clauses =
                [
                  {
                    CE.Cond_expand_clause.feature_requirement =
                      F.And [ F.feature_identifier FI.R7RS; F.library [ "a"; "b"; "c" ] ];
                    expression = parse "5";
                  };
                ];
              else_expression = None;
            }
        in
        Alcotest.(check @@ result expand_t error_t) "simple" expected actual);
    Alcotest.test_case "cond-expand parser: parse or clause" `Quick (fun () ->
        let v = parse "(cond-expand ((or r7rs (library (a b c))) 5))" in
        let actual = P.parse v in
        let expected =
          Ok
            {
              CE.clauses =
                [
                  {
                    CE.Cond_expand_clause.feature_requirement =
                      F.Or [ F.feature_identifier FI.R7RS; F.library [ "a"; "b"; "c" ] ];
                    expression = parse "5";
                  };
                ];
              else_expression = None;
            }
        in
        Alcotest.(check @@ result expand_t error_t) "simple" expected actual);
    Alcotest.test_case "cond-expand parser: parse not clause" `Quick (fun () ->
        let v = parse "(cond-expand ((not r7rs) 5))" in
        let actual = P.parse v in
        let expected =
          Ok
            {
              CE.clauses =
                [
                  {
                    CE.Cond_expand_clause.feature_requirement = F.Not (F.feature_identifier FI.R7RS);
                    expression = parse "5";
                  };
                ];
              else_expression = None;
            }
        in
        Alcotest.(check @@ result expand_t error_t) "simple" expected actual);
    Alcotest.test_case "cond-expand parser: parse multiple clause" `Quick (fun () ->
        let v = parse "(cond-expand ((not r7rs) 5) (r7rs 3) ((library (a)) \"test\"))" in
        let actual = P.parse v in
        let expected =
          Ok
            {
              CE.clauses =
                [
                  {
                    CE.Cond_expand_clause.feature_requirement = F.Not (F.feature_identifier FI.R7RS);
                    expression = parse "5";
                  };
                  { CE.Cond_expand_clause.feature_requirement = F.feature_identifier FI.R7RS; expression = parse "3" };
                  { CE.Cond_expand_clause.feature_requirement = F.library [ "a" ]; expression = parse {|"test"|} };
                ];
              else_expression = None;
            }
        in
        Alcotest.(check @@ result expand_t error_t) "simple" expected actual);
    Alcotest.test_case "cond-expand parser: parse else clause" `Quick (fun () ->
        let v = parse "(cond-expand (else 3))" in
        let actual = P.parse v in
        let expected = Ok { CE.clauses = []; else_expression = Some (parse "3") } in
        Alcotest.(check @@ result expand_t error_t) "simple" expected actual);
  ]
