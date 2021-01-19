module E = Environment
module S = Syntax

let evaluate_define env v =
  let open Lib.Result.Let_syntax in
  let* sym, v = match v with Syntax.Cons (Syntax.Symbol sym, v) -> Ok (sym, v) | _ -> Error "Invalid syntax" in
  let* value = Eval.eval env v in
  E.set env ~key:sym ~v:(S.Value value);
  Result.ok value

let eval_if env = function
  | S.Cons (cond, S.Cons (when_true, S.Cons (when_false, S.Empty_list))) -> (
      let open Lib.Result.Let_syntax in
      let* cond = Eval.eval env cond in
      match cond with S.False -> Eval.eval env when_false | _ -> Eval.eval env when_true )
  | _ as v -> Error (Printf.sprintf "Invalid syntax %s\n" @@ S.Data.to_string v)

let eval_set_force env v =
  let open Lib.Result.Let_syntax in
  let* sym, v =
    match v with S.Cons (S.Symbol sym, S.Cons (v, S.Empty_list)) -> Ok (sym, v) | _ -> Error "Invalid syntax"
  in
  let* value = Eval.eval env v in
  match E.replace env ~key:sym ~v:(S.Value value) with
  | None    -> Error (Printf.sprintf "%s is not defined" sym)
  | Some () -> Result.ok value
