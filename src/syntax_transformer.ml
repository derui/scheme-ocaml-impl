module E = Environment
module T = Type
module L = Syntax_rule_lib
module M = Syntax_rule_pattern_matcher
module Template = Syntax_rule_template

let eval_syntax_rules v =
  let open Lib.Result.Let_syntax in
  let* v, _ = L.Rule_parser.syntax_rules v in
  T.Syntax
    (fun _ data ->
      let matched = M.match_syntax_rules ~syntax_rules:v ~data in
      match matched with
      | None              -> Result.ok data
      | Some (matcher, _) -> Template.expand matcher v.ellipsis data |> Result.ok)
  |> Result.ok
