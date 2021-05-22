(** This module provides evaluation process by context. *)

module T = Type
module C = Continuation
module E = Environment
module ES = Eval_stack

type result =
  [ `Value       of T.data
  | `Expand      of T.data
  | `Cont        of T.data
  | `Call_syntax of T.special_form * T.data
  ]

module type S = sig
  val eval : stack:T.data -> env:T.env -> expr:T.data -> result T.evaluation_result
  (** [eval ~stack ~env ~expr] return a result of evaluation for expr. Evaluator can make decision with stack that is
      evaluated value list before current evaluation. *)
end

module Step_evaluator : S = struct
  let is_primitive = function T.Number _ | True | False | Empty_list | Scheme_string _ -> true | _ -> false

  let eval_symbol env sym =
    match E.get env ~key:sym with None -> T.raise_error @@ Printf.sprintf "%s is not bound" sym | Some v -> Ok v

  let eval_primitive v = Ok v

  (* evaluate a nested expression of expression *)
  let eval ~stack:_ ~env ~expr =
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
        match v with
        | T.Syntax form -> Ok (`Call_syntax (form, arg))
        | T.Macro f     ->
            let* v = f arg in
            Ok (`Expand v)
        | _             -> Ok (`Cont expr))
    | T.Cons _ -> Ok (`Cont expr)
    | _ as v -> T.raise_error (Printf.sprintf "Can not handle expression now... %s" @@ Printer.print v)
end

module Syntax_if_evaluator : S = struct
  (* evaluate a if syntax *)
  let eval ~stack ~env ~expr =
    match stack with
    | T.Empty_list                         -> Step_evaluator.eval ~stack ~env ~expr
    | T.Cons (v, T.Empty_list)             -> if T.is_false v then Ok (`Value expr)
                                              else Step_evaluator.eval ~stack ~env ~expr
    | T.Cons (_, T.Cons (v, T.Empty_list)) ->
        if T.is_true v then Ok (`Value expr) else Step_evaluator.eval ~stack ~env ~expr
    | _                                    -> T.raise_syntax_error "Invalid syntax"
end

module Syntax_define_evaluator : S = struct
  (* evaluate a if syntax *)
  let eval ~stack ~env ~expr =
    match stack with T.Empty_list -> Ok (`Value expr) | _ -> Step_evaluator.eval ~stack ~env ~expr
end

module Syntax_set_force_evaluator : S = struct
  (* evaluate a if syntax *)
  let eval ~stack ~env ~expr =
    match stack with T.Empty_list -> Ok (`Value expr) | _ -> Step_evaluator.eval ~stack ~env ~expr
end
