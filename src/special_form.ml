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
  let open Lib.Result.Let_syntax in
  (* unwrap first *)
  match v with
  | T.Cons ((Cons _ as v), T.Empty_list) ->
      let symbol_name = ref 0 in
      let get_sym_name () =
        incr symbol_name;
        Printf.sprintf "%dquasiquote" !symbol_name
      in
      let rec eval_quasiquote' accum variables v =
        match v with
        | T.Empty_list ->
            let* v = Primitive_op.List_op.reverse accum in
            Ok (v, List.rev variables)
        | Cons ((Cons (Symbol "quasiquote", _) as body), rest) -> eval_quasiquote' (T.Cons (body, accum)) variables rest
        | Cons (Cons (Symbol "unquote", body), rest) when unquote_binded env ->
            let sym_name = get_sym_name () in
            let variables = (sym_name, body) :: variables in
            eval_quasiquote' (T.Cons (T.Symbol sym_name, accum)) variables rest
        | Cons ((Cons (Symbol "unquote", _) as body), rest) when not @@ unquote_binded env ->
            eval_quasiquote' (T.Cons (T.Cons (T.Symbol "quote", body), accum)) variables rest
        | Cons ((Cons _ as v), rest) ->
            let* v, variables = eval_quasiquote' T.Empty_list variables v in
            eval_quasiquote' (T.Cons (v, accum)) variables rest
        | Cons (v, rest) -> eval_quasiquote' (T.Cons (v, accum)) variables rest
        | _ ->
            let* accum = Primitive_op.List_op.reverse accum in
            Ok (T.Cons (accum, v), List.rev variables)
      in
      let* body, variables = eval_quasiquote' Empty_list [] v in
      let argument_formal = D.Argument_formal.Fixed (List.map fst variables) in
      let arguments = List.map snd variables |> Internal_lib.list_to_scheme_list in
      Ok (T.Cons (T.Closure { env; argument_formal; body }, arguments))
  | T.Cons (v, T.Empty_list)             ->
      let argument_formal = D.Argument_formal.Fixed [] in
      Ok (T.Cons (T.Closure { env; argument_formal; body = v }, T.Empty_list))
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
