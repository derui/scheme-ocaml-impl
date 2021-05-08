(** Eval_environment have whole pointers and values, and function to evaluate between context and continuation. *)

module T = Type
module E = Eval_stack

type state =
  | Finish_evaluation of T.data
  | Start
  | Continue

type t = {
  mutable continuation_pointer : Continuation.t;
  mutable current_stack : Eval_stack.t;
  mutable current_env : T.binding Environment.t;
  mutable next_proc : state;
}

let next t = match t.next_proc with Finish_evaluation v -> `Finished v | _ -> `Continue

let make ~env expr =
  let stack = E.make ~kind:In_expression expr in
  let continuation = Continuation.make ~stack ~previous_continuation:None ~env in
  {
    continuation_pointer = continuation;
    current_stack = E.make ~kind:In_expression expr;
    next_proc = Start;
    current_env = env;
  }

let push_expr_continuation t expr =
  let new_stack = E.make ~kind:In_expression expr in
  let new_env = Environment.make ~parent_env:t.current_env [] in
  let new_cont =
    Continuation.make ~previous_continuation:(Some t.continuation_pointer) ~env:t.current_env
      ~stack:(E.clone t.current_stack)
  in
  t.continuation_pointer <- new_cont;
  t.current_stack <- new_stack;
  t.current_env <- new_env

let push_closure_continuation t env body =
  let new_stack = E.make ~kind:In_closure body in
  let new_cont =
    Continuation.make ~previous_continuation:(Some t.continuation_pointer) ~env:t.current_env
      ~stack:(E.clone t.current_stack)
  in
  t.continuation_pointer <- new_cont;
  t.current_stack <- new_stack;
  t.current_env <- env

let pop_continuation t =
  let value = E.evaluated_values t.current_stack in
  let old_stack = t.current_stack |> E.clone |> E.push_value ~value in
  let old_env = t.current_env in
  let old_cont = t.continuation_pointer.previous_continuation in
  let () =
    match old_cont with
    | None      -> t.next_proc <- Finish_evaluation value
    | Some cont ->
        t.continuation_pointer <- cont;
        t.current_stack <- old_stack;
        t.current_env <- old_env;
        t.next_proc <- Continue
  in
  t
