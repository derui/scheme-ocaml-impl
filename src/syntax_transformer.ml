module E = Environment
module T = Type
module L = Syntax_rule_lib
module M = Syntax_rule_pattern_matcher
module Template = Syntax_rule_template

let define_macro syntax_rules data =
  let matched = M.match_syntax_rules ~syntax_rules ~data in
  match matched with
  | None                          -> Result.ok data
  | Some (matcher, (_, template)) -> Template.expand matcher syntax_rules.ellipsis template |> Result.ok

let eval_syntax_rules v =
  let open Lib.Result.Let_syntax in
  let* v, _ = L.Rule_parser.syntax_rules v in
  T.Macro (fun data -> define_macro v data) |> Result.ok

let eval_define_syntax env v =
  let open Lib.Result.Let_syntax in
  match v with
  | T.Cons { car = T.Symbol sym; cdr = T.Cons { car = rest; cdr = T.Empty_list } } ->
      let* v, _ = L.Rule_parser.syntax_rules rest in
      let v = T.Macro (define_macro v) in
      E.set env ~key:sym ~v;
      Result.ok T.Empty_list
  | _ -> T.raise_syntax_error "Invalid syntax"
