module T = Type
module D = Data_type
module E = Environment
module C = Eval_context
module S = Eval_stack
module EP = Execution_pointer

module Evaluation_status = struct
  type evaluating_for =
    | For_syntax      of T.special_form
    | For_closure
    | For_application
    | For_expression

  type t = {
    stack : Eval_stack.t;
    env : T.env;
    mutable execution_pointer : Execution_pointer.t;
    evaluating_for : evaluating_for;
  }

  let clone t =
    {
      stack = Eval_stack.clone t.stack;
      env = t.env;
      execution_pointer = Execution_pointer.clone t.execution_pointer;
      evaluating_for = t.evaluating_for;
    }

  let execution_pointer t = t.execution_pointer

  let set_execution_pointer ep t = t.execution_pointer <- ep

  let is_syntax { evaluating_for; _ } = match evaluating_for with For_syntax _ -> true | _ -> false
end

let eval_apply continuation stack =
  let open Lib.Result.Infix in
  let open Lib.Result.Let_syntax in
  let rec argument_to_list evaled arg =
    match arg with
    | T.Empty_list      -> List.rev evaled |> Result.ok
    | Cons { car; cdr } -> argument_to_list (car :: evaled) cdr
    | _                 -> T.raise_error "argument is not proper list"
  in
  let to_binding_arguments formal arguments data =
    match formal with
    | D.Argument_formal.Any sym    -> Ok [ (sym, Internal_lib.list_to_scheme_list arguments) ]
    | Fixed symbols                ->
        let* arguments = argument_to_list [] data in
        List.map2 (fun sym v -> (sym, v)) symbols arguments |> Result.ok
    | Fixed_and_any (symbols, sym) ->
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

  let* values = S.evaluated_values stack |> Internal_lib.reverse in
  let* operator, args =
    match values with
    | T.Cons { car = operator; cdr = args } -> Ok (operator, args)
    | _                                     -> T.raise_error
                                                 (Printf.sprintf "Invalid application: %s" @@ Printer.print values)
  in
  let module I = (val continuation : C.Instance with type status = Evaluation_status.t) in
  let cont v = I.(pop_continuation instance v) in
  match operator with
  | T.Primitive_fun (formal, f)            ->
      Printf.printf "call primitive\n";
      let* validated_args = Internal_lib.validate_arguments formal args in
      let* value = f cont validated_args in
      Ok (`Value value)
  | Closure { env; argument_formal; body } ->
      let* data = Internal_lib.validate_arguments argument_formal args in
      let* arguments = argument_to_list [] data in
      let* zipped_arguments = to_binding_arguments argument_formal arguments data in
      let env = E.make ~parent_env:env zipped_arguments in
      Ok (`Call_closure (env, body))
  | _                                      -> T.raise_error @@ Printf.sprintf "need closure: %s"
                                              @@ Printer.print operator

let eval ~runtime ~env expr =
  let open Lib.Result.Let_syntax in
  let open Lib.Result.Infix in
  let execution_pointer = EP.make (T.cons expr T.Empty_list) in
  let context =
    C.make
      (module Evaluation_status)
      { Evaluation_status.stack = S.make (); env; evaluating_for = For_expression; execution_pointer }
  in
  let module I = (val context : C.Instance with type status = Evaluation_status.t) in
  let rec apply_step env stack v (module E : Evaluator.S) =
    match v with
    | `Value value             ->
        Printf.printf "apply_step: %s\n" @@ Printer.print value;
        I.(update_status instance) ~f:(fun t -> { t with Evaluation_status.stack = S.push_value stack ~value })
        |> Result.ok
    | `Expand expr             ->
        let stack' = stack |> S.evaluated_values in
        let* v = E.eval ~stack:stack' ~env ~expr in
        apply_step env stack v (module E)
    | `Cont expr               ->
        Printf.printf "cont: %s\n" @@ Printer.print expr;
        let new_stack = S.make () in
        let new_env = Environment.make ~parent_env:env [] in
        let execution_pointer = EP.make expr in
        let status' =
          { Evaluation_status.env = new_env; stack = new_stack; evaluating_for = For_application; execution_pointer }
        in
        I.(push_continuation instance status') |> Result.ok
    | `Call_syntax (form, arg) -> (
        match form with
        | T.S_quasiquote ->
            let* expr = Special_form.eval_quasiquote runtime env arg in
            let stack' = stack |> S.evaluated_values in
            let* v = E.eval ~stack:stack' ~env ~expr in
            apply_step env stack v (module E)
        | _              ->
            let new_stack = S.make () in
            let execution_pointer = EP.make arg in
            let status' =
              { Evaluation_status.env; stack = new_stack; evaluating_for = For_syntax form; execution_pointer }
            in
            I.(push_continuation instance status') |> Result.ok)
  in
  let push_or_pop () =
    let status = I.(status instance) in
    match status.evaluating_for with
    (* if no stack and it is end of instruction, return value *)
    | Evaluation_status.For_closure ->
        Printf.printf "total stack: %s\n" @@ Printer.print @@ (status.stack |> S.evaluated_values);
        let value = match status.stack |> S.evaluated_values with T.Cons { car = v; _ } -> v | _ as v -> v in
        Printf.printf "pop value: %s\n" @@ Printer.print value;
        I.(pop_continuation instance value) |> Result.ok
    | Evaluation_status.For_application -> (
        let* evaled = eval_apply (module I) status.stack in
        match evaled with
        | `Value value              -> I.(pop_continuation instance value) |> Result.ok
        | `Call_closure (env, body) ->
            Printf.printf "push closure: %s\n" @@ Printer.print body;
            let new_stack = S.make () in
            let execution_pointer = EP.make body in
            let new_status =
              { Evaluation_status.stack = new_stack; env; evaluating_for = For_closure; execution_pointer }
            in
            I.(push_continuation instance new_status) |> Result.ok)
    | For_syntax form -> (
        let open Lib.Result.Infix in
        let* evaluated = status.stack |> S.evaluated_values |> Internal_lib.reverse in
        let pop v = I.(pop_continuation instance v) |> Result.ok in
        match form with
        | T.S_if          -> Special_form.eval_if runtime status.env evaluated >>= pop
        | T.S_define      -> Special_form.eval_define runtime status.env evaluated >>= pop
        | T.S_set_force   -> Special_form.eval_set_force runtime status.env evaluated >>= pop
        | T.S_quote       -> Special_form.eval_quote runtime status.env evaluated >>= pop
        | T.S_lambda      -> Special_form.eval_lambda runtime status.env evaluated >>= pop
        | T.S_unquote     -> Special_form.eval_unquote runtime status.env evaluated >>= pop
        | T.S_cond_expand -> Special_form.eval_cond_expand runtime status.env evaluated >>= pop
        | _               -> T.raise_syntax_error "Invalid evaluation process")
    | For_expression ->
        let value = match status.stack |> S.evaluated_values with T.Cons { car = v; _ } -> v | _ as v -> v in
        I.(pop_continuation instance value) |> Result.ok
  in

  let rec eval_loop () =
    let status = I.(status instance) in
    let env = status.env and stack = status.stack in
    let module E =
    (val match status.Evaluation_status.evaluating_for with
         | For_syntax T.S_if          -> (module Evaluator.Syntax_if_evaluator : Evaluator.S)
         | For_syntax T.S_define      -> (module Evaluator.Syntax_define_evaluator : Evaluator.S)
         | For_syntax T.S_set_force   -> (module Evaluator.Syntax_set_force_evaluator : Evaluator.S)
         | For_syntax T.S_quote       -> (module Evaluator.Syntax_quote_evaluator : Evaluator.S)
         | For_syntax T.S_lambda      -> (module Evaluator.Syntax_quote_evaluator : Evaluator.S)
         | For_syntax T.S_unquote     -> (module Evaluator.Syntax_quote_evaluator : Evaluator.S)
         | For_syntax T.S_cond_expand -> (module Evaluator.Syntax_quote_evaluator : Evaluator.S)
         | _                          -> (module Evaluator.Step_evaluator : Evaluator.S))
    in
    match I.(next instance) with
    | `Finished v      ->
        let v' = match S.evaluated_values v.stack with T.Cons { car = v; _ } -> v | _ as v -> v in
        Result.ok v'
    | `Continue expr   ->
        let stack' = stack |> S.evaluated_values in
        let* v = E.eval ~stack:stack' ~env ~expr in
        apply_step env stack v (module E) >>= eval_loop
    | `End_instruction -> push_or_pop () >>= eval_loop
    | `Popped value    ->
        I.(update_status instance) ~f:(fun t -> { t with stack = S.push_value t.stack ~value })
        |> Result.ok >>= eval_loop
  in
  eval_loop ()
