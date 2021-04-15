module E = Environment
module T = Type

let eval_define env v =
  let open Lib.Result.Let_syntax in
  let* sym, v =
    match v with T.Cons (Symbol sym, Cons (v, Empty_list)) -> Ok (sym, v) | _ -> T.raise_syntax_error "Invalid syntax"
  in
  let* value = Eval.eval env v in
  E.set env ~key:sym ~v:(T.Value value);
  Result.ok value

let eval_if env = function
  | T.Cons (cond, Cons (when_true, Cons (when_false, Empty_list))) -> (
      let open Lib.Result.Let_syntax in
      let* cond = Eval.eval env cond in
      match cond with T.False -> Eval.eval env when_false | _ -> Eval.eval env when_true )
  | _ as v -> T.raise_error ~irritants:[ v ] (Printf.sprintf "Invalid syntax %s\n" @@ Printer.print v)

let eval_set_force env v =
  let open Lib.Result.Let_syntax in
  let* sym, v =
    match v with T.Cons (Symbol sym, Cons (v, Empty_list)) -> Ok (sym, v) | _ -> T.raise_syntax_error "Invalid syntax"
  in
  let* value = Eval.eval env v in
  match E.replace env ~key:sym ~v:(T.Value value) with
  | None    -> T.raise_error ~irritants:[ v ] (Printf.sprintf "%s is not defined" sym)
  | Some () -> Result.ok value

let rec eval_sequence env bodies =
  let open Lib.Result.Let_syntax in
  match bodies with
  | T.Empty_list         -> Ok T.Empty_list
  | Cons (v, Empty_list) -> Eval.eval env v
  | Cons (v, bodies)     ->
      let* _ = Eval.eval env v in
      eval_sequence env bodies
  | _                    -> Ok T.Empty_list

let eval_lambda env v =
  let open Lib.Result.Let_syntax in
  match v with
  | T.Cons (Symbol sym, body) -> Ok (T.Closure { env; argument_formal = T.Any sym; body })
  | Cons (bindings, body)     ->
      let rec get_argument_symbols symbols rest =
        match rest with
        | T.Empty_list            -> List.rev symbols |> (fun v -> (v, None)) |> Result.ok
        | Symbol sym              -> List.rev symbols |> (fun v -> (v, Some sym)) |> Result.ok
        | Cons (Symbol sym, rest) -> get_argument_symbols (sym :: symbols) rest
        | _                       -> T.raise_error @@ Printf.sprintf "Syntax error: malformed let: %s" @@ Printer.print v
      in
      let* arguments, rest_variable = get_argument_symbols [] bindings in
      let argument_formal =
        match rest_variable with None -> T.Fixed arguments | Some sym -> T.Fixed_and_any (arguments, sym)
      in
      Ok (T.Closure { env; argument_formal; body })
  | _                         -> T.raise_error
                                 @@ Printf.sprintf "Syntax error: need argument list: %s"
                                 @@ Printer.print v

let eval_quasiquote env v =
  let open Lib.Result.Let_syntax in
  (* unwrap first *)
  match v with
  | T.Cons (v, Empty_list) -> (
      match v with
      | Cons _ ->
          let rec eval_quasiquote' accum v =
            match v with
            | T.Empty_list -> Primitive_op.List_op.reverse accum
            | Cons ((Cons (Symbol "unquote", _) as body), rest) ->
                let* body = Eval.eval env body in
                eval_quasiquote' (T.Cons (body, accum)) rest
            | Cons (v, rest) -> eval_quasiquote' (T.Cons (v, accum)) rest
            | _ ->
                let* accum = Primitive_op.List_op.reverse accum in
                Ok (T.Cons (accum, v))
          in
          eval_quasiquote' Empty_list v
      | _      -> Ok v )
  | _                      -> T.raise_error @@ Printf.sprintf "Invalid syntax: quasiquote: %s" @@ Printer.print v

let eval_unquote env v =
  (* unwrap first *)
  match v with
  | T.Cons (v, Empty_list) -> Eval.eval env v
  | _                      -> T.raise_error @@ Printf.sprintf "Invalid syntax: unquote: %s" @@ Printer.print v

let eval_quote _ v =
  let open Lib.Result.Infix in
  (* unwrap first *)
  match v with
  | T.Cons (v, Empty_list) -> (
      match v with
      | Cons _ ->
          let rec eval_quote' accum v =
            match v with
            | T.Empty_list   -> Primitive_op.List_op.reverse accum
            | Cons (v, rest) -> eval_quote' (Cons (v, accum)) rest
            | _              -> Primitive_op.List_op.reverse accum >>| fun accum -> T.Cons (accum, v)
          in
          eval_quote' Empty_list v
      | _      -> Ok v )
  | _                      -> T.raise_error @@ Printf.sprintf "Invalid syntax: quote: %s" @@ Printer.print v

module Export = struct
  let eval_define = eval_define

  let eval_if = eval_if

  let eval_set_force = eval_set_force

  let eval_lambda = eval_lambda

  let eval_quasiquote = eval_quasiquote

  let eval_unquote = eval_unquote

  let eval_quote = eval_quote
end
