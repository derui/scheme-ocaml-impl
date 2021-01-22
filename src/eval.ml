module S = Syntax
module E = Environment

let is_primitive = function S.Number _ | S.True | S.False -> true | _ -> false

let eval_symbol env sym =
  match E.get env ~key:sym with
  | None   -> S.raise_error @@ Printf.sprintf "%s is not bound" sym
  | Some v -> ( match v with S.Value v -> Ok v | S.Special_form form -> Ok (S.Closure form) )

let eval_primitive _ v = Ok v

let rec eval env = function
  | Syntax.Symbol sym -> eval_symbol env sym
  | _ as v when is_primitive v -> eval_primitive env v
  | Syntax.Cons (_, _) as v -> eval_list env v
  | _ as v -> S.raise_error (Printf.sprintf "Can not handle expression now... %s" @@ Syntax.Data.to_string v)

and eval_list env v =
  let open Lib.Result.Let_syntax in
  match v with
  | S.Cons (S.Symbol sym, arg) -> (
      match E.get env ~key:sym with
      | None                       -> S.raise_error @@ Printf.sprintf "%s is not bound" sym
      | Some (S.Value v)           -> eval_combo env v arg
      | Some (S.Special_form form) -> Lambda.eval env form arg )
  | S.Cons (car, cdr)          ->
      let* car = eval env car in
      let* arg = eval_list env cdr in
      eval_combo env car arg
  | _                          -> S.raise_error @@ Printf.sprintf "Invalid syntax: %s" @@ S.Data.to_string v

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
  let* arg = Buildin_op.List_op.reverse env arg in
  match f with
  | Syntax.Native_fun f -> Lambda.eval env f arg
  | Syntax.Closure f    -> Lambda.eval env f arg
  | _                   -> failwith @@ Printf.sprintf "Illegal path: %s" @@ S.Data.to_string arg
