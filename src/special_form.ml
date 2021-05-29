module E = Environment
module T = Type
module D = Data_type

let eval_define env v =
  let open Lib.Result.Let_syntax in
  let* sym, v =
    match v with T.Cons (Symbol sym, Cons (v, Empty_list)) -> Ok (sym, v) | _ -> T.raise_syntax_error "Invalid syntax"
  in
  E.set env ~key:sym ~v;
  Result.ok v

let eval_if _ = function
  | T.Cons (cond, Cons (when_true, Cons (when_false, Empty_list))) -> (
      match cond with T.False -> Ok when_false | _ -> Ok when_true)
  | T.Cons (cond, Cons (when_true, Empty_list)) -> ( match cond with T.False -> Ok T.Undef | _ -> Ok when_true)
  | _ as v -> T.raise_error ~irritants:[ v ] (Printf.sprintf "Invalid syntax %s\n" @@ Printer.print v)

let eval_set_force env v =
  let open Lib.Result.Let_syntax in
  let* sym, v =
    match v with T.Cons (Symbol sym, Cons (v, Empty_list)) -> Ok (sym, v) | _ -> T.raise_syntax_error "Invalid syntax"
  in
  match E.replace env ~key:sym ~v with
  | None    -> T.raise_error ~irritants:[ v ] (Printf.sprintf "%s is not defined" sym)
  | Some () -> Result.ok v

let eval_lambda env v =
  let open Lib.Result.Let_syntax in
  match v with
  | T.Cons (Symbol sym, body) -> Ok (T.Closure { env; argument_formal = D.Argument_formal.Any sym; body })
  | Cons (bindings, body)     ->
      let rec get_argument_symbols symbols rest =
        match rest with
        | T.Empty_list            -> List.rev symbols |> (fun v -> (v, None)) |> Result.ok
        | Symbol sym              -> List.rev symbols |> (fun v -> (v, Some sym)) |> Result.ok
        | Cons (Symbol sym, rest) -> get_argument_symbols (sym :: symbols) rest
        | _                       -> T.raise_syntax_error
                                     @@ Printf.sprintf "Syntax error: malformed let: %s"
                                     @@ Printer.print v
      in
      let* arguments, rest_variable = get_argument_symbols [] bindings in
      let argument_formal =
        match rest_variable with
        | None     -> D.Argument_formal.Fixed arguments
        | Some sym -> D.Argument_formal.Fixed_and_any (arguments, sym)
      in
      Ok (T.Closure { env; argument_formal; body })
  | _                         -> T.raise_syntax_error
                                 @@ Printf.sprintf "Syntax error: need argument list: %s"
                                 @@ Printer.print v

let unquote_binded env = match E.get env ~key:"unquote" with None -> false | Some v -> v = T.Syntax T.S_unquote

let eval_quasiquote env v =
  (* unwrap first *)
  let rec list_to_cons accum list =
    let f = T.Primitive_fun Primitive_op.List_op.Export.cons in
    let cons car cdr = T.Cons (f, T.Cons (car, T.Cons (cdr, T.Empty_list))) in
    match list with [] -> accum | car :: rest -> list_to_cons (cons car accum) rest
  in
  let quote v =
    let f = T.Symbol "quote" in
    T.Cons (f, T.Cons (v, T.Empty_list))
  in
  match v with
  | T.Cons ((Cons _ as v), T.Empty_list) ->
      let rec eval_quasiquote' accum v =
        match v with
        | T.Empty_list -> list_to_cons T.Empty_list accum
        | Cons ((Cons (Symbol "quasiquote", _) as body), rest) -> eval_quasiquote' (quote body :: accum) rest
        | Cons (Cons (Symbol "unquote", T.Cons (body, T.Empty_list)), rest) when unquote_binded env ->
            eval_quasiquote' (body :: accum) rest
        | Cons ((Cons (Symbol "unquote", _) as body), rest) when not @@ unquote_binded env ->
            eval_quasiquote' (quote body :: accum) rest
        | Cons ((Cons _ as v), rest) ->
            let v = eval_quasiquote' [] v in
            eval_quasiquote' (v :: accum) rest
        | Cons (v, rest) -> eval_quasiquote' (quote v :: accum) rest
        | _ -> list_to_cons T.Empty_list (quote v :: accum)
      in
      let body = eval_quasiquote' [] v in
      Printf.printf "expanded: %s\n" @@ Printer.print body;
      Ok body
  | T.Cons (v, T.Empty_list)             -> Ok (quote v)
  | _                                    -> T.raise_syntax_error
                                            @@ Printf.sprintf "Invalid syntax: quasiquote: %s"
                                            @@ Printer.print v

let eval_unquote _ v =
  match v with
  | T.Cons (v, Empty_list) -> Ok v
  | _                      -> T.raise_syntax_error @@ Printf.sprintf "Invalid syntax: unquote: %s" @@ Printer.print v

let eval_quote _ v =
  match v with
  | T.Cons (v, T.Empty_list) -> Ok v
  | _                        -> T.raise_syntax_error @@ Printf.sprintf "malformed quote: %s"
                                @@ Printer.print (T.Cons (T.Symbol "quote", v))

module Export = struct
  let eval_define = eval_define

  let eval_if = eval_if

  let eval_set_force = eval_set_force

  let eval_lambda = eval_lambda

  let eval_quote = eval_quote
end
