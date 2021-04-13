module E = Environment
module T = Type
module L = Syntax_rule_lib
module M = Syntax_rule_pattern_matcher
module Template = Syntax_rule_template

let define_macro syntax_rules env data =
  let matched = M.match_syntax_rules ~syntax_rules ~data in
  match matched with
  | None                          -> Result.ok data
  | Some (matcher, (_, template)) ->
      let expanded = Template.expand matcher syntax_rules.ellipsis template in
      Eval.eval env expanded

let eval_syntax_rules v =
  let open Lib.Result.Let_syntax in
  let* v, _ = L.Rule_parser.syntax_rules v in
  T.Syntax (fun env data -> define_macro v env data) |> Result.ok

let eval_define_syntax env v =
  let open Lib.Result.Let_syntax in
  match v with
  | T.Cons (T.Symbol sym, T.Cons (rest, T.Empty_list)) ->
      let* v, _ = L.Rule_parser.syntax_rules rest in
      let v = T.Macro (define_macro v) in
      E.set env ~key:sym ~v;
      Result.ok T.Empty_list
  | _ -> Error "Invalid syntax"

let eval_let_syntax env v =
  let open Lib.Result.Let_syntax in
  let rec loop bindings v =
    match v with
    | T.Cons (Cons (Symbol sym, syntax), rest) -> (
        let* v = Eval.eval env syntax in
        match v with T.Syntax v -> loop ((sym, T.Macro v) :: bindings) rest | _ -> Error "Need syntax" )
    | T.Empty_list -> Ok bindings
    | _ -> Error "Invalid syntax"
  in
  let* bindings = loop [] v in
  let keywords = List.map fst bindings |> List.sort_uniq Stdlib.compare in
  if List.length bindings <> List.length keywords then Error "duplicate keywords"
  else (
    List.iter (fun (keyword, syntax) -> E.set env ~key:keyword ~v:syntax) bindings;
    Ok T.Empty_list )
