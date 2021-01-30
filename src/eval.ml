module S = Syntax
module E = Environment

let is_primitive = function S.Number _ | S.True | S.False | S.Empty_list -> true | _ -> false

let eval_symbol env sym =
  match E.get env ~key:sym with
  | None   -> S.raise_error @@ Printf.sprintf "%s is not bound" sym
  | Some v -> ( match v with S.Value v -> Ok v | S.Special_form form -> Ok (S.Syntax_fun form) )

let eval_primitive _ v = Ok v

let rec eval env = function
  | Syntax.Symbol sym -> eval_symbol env sym
  | _ as v when is_primitive v -> eval_primitive env v
  | Syntax.Cons (_, _) as v -> eval_list env v
  | _ as v -> S.raise_error (Printf.sprintf "Can not handle expression now... %s" @@ Printer.print v)

and eval_sequence env bodies =
  let open Lib.Result.Let_syntax in
  match bodies with
  | S.Empty_list             -> Ok S.Empty_list
  | S.Cons (v, S.Empty_list) -> eval env v
  | S.Cons (v, bodies)       ->
      let* _ = eval env v in
      eval_sequence env bodies
  | _                        -> Ok S.Empty_list

and eval_list env v =
  let open Lib.Result.Let_syntax in
  match v with
  | S.Cons (S.Symbol sym, arg) -> (
      let* v = eval_symbol env sym in
      match v with S.Syntax_fun f -> f env arg | _ -> eval_combo env v arg )
  | S.Cons (car, cdr)          ->
      let* car = eval env car in
      let* arg = eval_list env cdr in
      eval_combo env car arg
  | _                          -> S.raise_error @@ Printf.sprintf "Invalid syntax: %s" @@ Printer.print v

and eval_combo env f arg =
  let open Lib.Result.Let_syntax in
  let rec eval_arguments evaled arg =
    match arg with
    | S.Empty_list      -> Ok evaled
    | S.Cons (car, cdr) ->
        let* car = eval env car in
        eval_arguments (S.Cons (car, evaled)) cdr
    | _                 -> S.raise_error "argument is not proper list"
  in
  let* arg = eval_arguments S.Empty_list arg in
  let* arg = Primitive_op.List_op.reverse arg in
  match f with
  | Syntax.Primitive_fun _ | Syntax.Closure _ -> eval_apply f arg
  | _ -> failwith @@ Printf.sprintf "Illegal path: %s" @@ Printer.print arg

and eval_apply closure data =
  let open Lib.Result.Infix in
  let open Lib.Result.Let_syntax in
  let rec argument_to_list evaled arg =
    match arg with
    | S.Empty_list      -> List.rev evaled |> Result.ok
    | S.Cons (car, cdr) -> argument_to_list (car :: evaled) cdr
    | _                 -> S.raise_error "argument is not proper list"
  in
  let to_binding_arguments formal arguments =
    match formal with
    | S.Fixed symbols                ->
        let* arguments = argument_to_list [] data in
        List.map2 (fun sym v -> (sym, S.Value v)) symbols arguments |> Result.ok
    | S.Any sym                      -> Ok [ (sym, S.Value (Internal_lib.list_to_scheme_list arguments)) ]
    | S.Fixed_and_any (symbols, sym) ->
        let* arguments = argument_to_list [] data >>= fun v -> Ok (Array.of_list v) in
        let symbol_len = List.length symbols in
        let rest_length = max 0 (Array.length arguments - symbol_len) in

        let arguments_for_fixed = Array.sub arguments 0 symbol_len |> Array.to_list in
        let bindings = List.map2 (fun sym v -> (sym, S.Value v)) symbols arguments_for_fixed
        and rest_binding =
          Array.sub arguments symbol_len rest_length |> Array.to_list |> Internal_lib.list_to_scheme_list
        in
        (sym, S.Value rest_binding) :: bindings |> Result.ok
  in

  match closure with
  | S.Primitive_fun (formal, f) ->
      let* validated_args = Internal_lib.validate_arguments formal data in
      f validated_args
  | S.Closure { env; argument_formal; body } ->
      let* data = Internal_lib.validate_arguments argument_formal data in
      let* arguments = argument_to_list [] data in
      let* zipped_arguments = to_binding_arguments argument_formal arguments in
      let env = E.make ~parent_env:env zipped_arguments in
      eval_sequence env body
  | _ -> S.raise_error @@ Printf.sprintf "need closure: %s" @@ Printer.print closure
