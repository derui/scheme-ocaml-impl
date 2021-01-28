module E = Environment
module S = Syntax

let eval_define env v =
  let open Lib.Result.Let_syntax in
  let* sym, v =
    match v with
    | Syntax.Cons (Syntax.Symbol sym, S.Cons (v, S.Empty_list)) -> Ok (sym, v)
    | _ -> Error "Invalid syntax"
  in
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

let eval_lambda env v =
  let open Lib.Result.Let_syntax in
  match v with
  | S.Cons (S.Symbol sym, body) -> Ok (S.Closure { env; argument_formal = S.Any sym; body })
  | S.Cons (bindings, body)     ->
      let rec get_argument_symbols symbols rest =
        match rest with
        | S.Empty_list                -> List.rev symbols |> (fun v -> (v, None)) |> Result.ok
        | S.Symbol sym                -> List.rev symbols |> (fun v -> (v, Some sym)) |> Result.ok
        | S.Cons (S.Symbol sym, rest) -> get_argument_symbols (sym :: symbols) rest
        | _                           -> S.raise_error
                                         @@ Printf.sprintf "Syntax error: malformed let: %s"
                                         @@ S.Data.to_string v
      in
      let* arguments, rest_variable = get_argument_symbols [] bindings in
      let argument_formal =
        match rest_variable with None -> S.Fixed arguments | Some sym -> S.Fixed_and_any (arguments, sym)
      in
      Ok (S.Closure { env; argument_formal; body })
  | _                           -> S.raise_error
                                   @@ Printf.sprintf "Syntax error: need argument list: %s"
                                   @@ S.Data.to_string v

let eval_quasiquote env v =
  let open Lib.Result.Let_syntax in
  (* unwrap first *)
  match v with
  | S.Cons (v, S.Empty_list) -> (
      match v with
      | S.Cons _ ->
          let rec eval_quasiquote' accum v =
            match v with
            | S.Empty_list -> Primitive_op.List_op.reverse accum
            | S.Cons ((S.Cons (S.Symbol "unquote", _) as body), rest) ->
                let* body = Eval.eval env body in
                eval_quasiquote' (S.Cons (body, accum)) rest
            | S.Cons (v, rest) -> eval_quasiquote' (S.Cons (v, accum)) rest
            | _ ->
                let* accum = Primitive_op.List_op.reverse accum in
                Ok (S.Cons (accum, v))
          in
          eval_quasiquote' S.Empty_list v
      | _        -> Ok v )
  | _                        -> S.raise_error @@ Printf.sprintf "Invalid syntax: quasquote: %s" @@ S.Data.to_string v

let eval_unquote env v =
  (* unwrap first *)
  match v with
  | S.Cons (v, S.Empty_list) -> Eval.eval env v
  | _                        -> S.raise_error @@ Printf.sprintf "Invalid syntax: unquote: %s" @@ S.Data.to_string v

module Export = struct
  let eval_define = eval_define

  let eval_if = eval_if

  let eval_set_force = eval_set_force

  let eval_let = eval_let

  let eval_lambda = eval_lambda

  let eval_quasiquote = eval_quasiquote

  let eval_unquote = eval_unquote
end
