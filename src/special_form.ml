module E = Environment
module S = Syntax

let eval_define env v =
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

let rec eval_sequence env bodies =
  let open Lib.Result.Let_syntax in
  match bodies with
  | S.Empty_list             -> Ok S.Empty_list
  | S.Cons (v, S.Empty_list) -> Eval.eval env v
  | S.Cons (v, bodies)       ->
      let* _ = Eval.eval env v in
      eval_sequence env bodies
  | _                        -> Ok S.Empty_list

let eval_let env v =
  let open Lib.Result.Let_syntax in
  let* bindings, body =
    match v with
    | S.Cons (bindings, body) ->
        let rec get_bindings bindings rest =
          match rest with
          | S.Empty_list -> Ok bindings
          | S.Cons (S.Cons (S.Symbol sym, S.Cons (value, S.Empty_list)), rest) ->
              get_bindings ((sym, value) :: bindings) rest
          | _ -> S.raise_error @@ Printf.sprintf "Syntax error: malformed let: %s" @@ S.Data.to_string v
        in
        let* bindings = get_bindings [] bindings in
        Ok (bindings, body)
    | _                       -> S.raise_error @@ Printf.sprintf "Syntax error: need bindings: %s" @@ S.Data.to_string v
  in
  let* bindings =
    List.fold_left
      (fun accum (key, v) ->
        let* accum = accum in
        let* v = Eval.eval env v in
        (key, S.Value v) :: accum |> Result.ok)
      (Ok []) bindings
  in
  let new_env = E.make ~parent_env:env bindings in
  eval_sequence new_env body

module Export = struct
  let eval_define = (Some 2, eval_define)

  let eval_if = (Some 3, eval_if)

  let eval_set_force = (Some 2, eval_set_force)

  let eval_let = (Some 2, eval_let)
end
