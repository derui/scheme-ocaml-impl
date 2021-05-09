module T = Type
module D = Data_type
module E = Environment
module C = Eval_context
module S = Eval_stack

module Evaluation_status = struct
  type t = {
    stack : S.t;
    env : T.env;
  }

  let clone t = { stack = S.clone t.stack; env = t.env }
end

let is_primitive = function T.Number _ | True | False | Empty_list | Scheme_string _ -> true | _ -> false

let is_application = function T.Cons _ -> true | _ -> false

let eval_symbol env sym =
  match E.get env ~key:sym with None -> T.raise_error @@ Printf.sprintf "%s is not bound" sym | Some v -> Ok v

let eval_primitive v = Ok v

(* evaluate a nested expression of expression *)
let eval_step ~env expr =
  let open Lib.Result.Let_syntax in
  match expr with
  | T.Symbol sym ->
      let* ret = eval_symbol env sym in
      Ok (`Value ret)
  | _ as v when is_primitive v ->
      let* ret = eval_primitive v in
      Ok (`Value ret)
  (* special case for macro and syntax *)
  | T.Cons (Symbol sym, arg) -> (
      let* v = eval_symbol env sym in
      match v with T.Syntax f -> Ok (`Call_syntax (f, arg)) | T.Macro f -> Ok (`Expand (f arg)) | _ -> Ok (`Cont expr))
  | T.Cons _ -> Ok (`Cont expr)
  | _ as v -> T.raise_error (Printf.sprintf "Can not handle expression now... %s" @@ Printer.print v)

let eval_apply stack =
  let open Lib.Result.Infix in
  let open Lib.Result.Let_syntax in
  let rec argument_to_list evaled arg =
    match arg with
    | T.Empty_list    -> List.rev evaled |> Result.ok
    | Cons (car, cdr) -> argument_to_list (car :: evaled) cdr
    | _               -> T.raise_error "argument is not proper list"
  in
  let to_binding_arguments formal arguments data =
    match formal with
    | D.Argument_formal.Fixed symbols ->
        let* arguments = argument_to_list [] data in
        List.map2 (fun sym v -> (sym, v)) symbols arguments |> Result.ok
    | Any sym                         -> Ok [ (sym, Internal_lib.list_to_scheme_list arguments) ]
    | Fixed_and_any (symbols, sym)    ->
        let* arguments = argument_to_list [] data >>= fun v -> Ok (Array.of_list v) in
        let symbol_len = List.length symbols in
        let rest_length = max 0 (Array.length arguments - symbol_len) in

        let arguments_for_fixed = Array.sub arguments 0 symbol_len |> Array.to_list in
        let bindings = List.map2 (fun sym v -> (sym, v)) symbols arguments_for_fixed
        and rest_binding =
          Array.sub arguments symbol_len rest_length |> Array.to_list |> Internal_lib.list_to_scheme_list
        in
        (sym, rest_binding) :: bindings |> Result.ok
  in

  let* values = S.evaluated_values stack |> Primitive_op.List_op.reverse in
  match values with
  | T.Cons (operator, args) -> (
      match operator with
      | T.Primitive_fun (formal, f)            ->
          let* validated_args = Internal_lib.validate_arguments formal args in
          Ok (`Value (f validated_args))
      | Closure { env; argument_formal; body } ->
          let* data = Internal_lib.validate_arguments argument_formal args in
          let* arguments = argument_to_list [] data in
          let* zipped_arguments = to_binding_arguments argument_formal arguments data in
          let env = E.make ~parent_env:env zipped_arguments in
          Ok (`Call_closure (env, body))
      | _                                      -> T.raise_error @@ Printf.sprintf "need closure: %s"
                                                  @@ Printer.print operator)
  | _                       -> T.raise_error (Printf.sprintf "Invalid application: %s" @@ Printer.print values)

let eval ~env expr =
  let open Lib.Result.Let_syntax in
  if not @@ is_application expr then
    let* ret = eval_step ~env expr in
    match ret with `Value v -> Ok v | _ -> T.raise_error "Invalid evaluation path"
  else
    let stack = S.make ~kind:In_expression expr in
    let context = C.make (module Evaluation_status) { Evaluation_status.stack; env } in
    let module I = (val context : C.Instance with type status = Evaluation_status.t) in
    let push_or_pop () =
      let status = I.(status instance) in
      match S.kind status.stack with
      | S.In_closure    ->
          let value = S.evaluated_values status.stack in
          I.(pop_continuation instance);
          I.(update_status instance) ~f:(fun status ->
              { status with Evaluation_status.stack = S.push_value status.stack ~value })
          |> Result.ok
      | S.In_expression -> (
          let* evaled = eval_apply status.stack in
          match evaled with
          | `Value value              ->
              let* value = value in
              I.(pop_continuation instance);
              I.(update_status instance) ~f:(fun status ->
                  { status with Evaluation_status.stack = S.push_value status.stack ~value })
              |> Result.ok
          | `Call_closure (env, body) ->
              let new_stack = S.make ~kind:S.In_closure body in
              let new_status = { Evaluation_status.stack = new_stack; env } in
              I.(push_continuation instance new_status) |> Result.ok)
    in

    let apply_step env stack v =
      match v with
      | `Value value          ->
          I.(update_status instance) ~f:(fun t -> { t with Evaluation_status.stack = S.push_value stack ~value })
          |> Result.ok
      | `Expand expr          ->
          let* expr = expr in
          I.(update_status instance) ~f:(fun t -> { t with Evaluation_status.stack = S.replace_current stack expr })
          |> Result.ok
      | `Cont expr            ->
          let new_stack = S.make ~kind:In_expression expr in
          let new_env = Environment.make ~parent_env:env [] in
          let status' = Evaluation_status.{ env = new_env; stack = new_stack } in
          I.(push_continuation instance status') |> Result.ok
      | `Call_syntax (f, arg) -> (
          let* result = f env arg in
          match result with
          | `Macro expr  ->
              I.(update_status instance) ~f:(fun t -> { t with Evaluation_status.stack = S.replace_current stack expr })
              |> Result.ok
          | `Value value ->
              I.(update_status instance) ~f:(fun t -> { t with Evaluation_status.stack = S.push_value stack ~value })
              |> Result.ok)
    in

    let rec eval_loop () =
      let status = I.(status instance) in
      let env = status.env and stack = status.stack in
      match I.(next instance) with
      | `Finished v -> ( match S.evaluated_values v.stack with T.Cons (v, _) -> Result.ok v | _ as v -> Result.ok v)
      | `Continue   ->
          let expr = S.current stack in
          let* () =
            match expr with
            | None      -> push_or_pop ()
            | Some expr ->
                let* v = eval_step ~env expr in
                apply_step env stack v
          in
          eval_loop ()
    in
    eval_loop ()
