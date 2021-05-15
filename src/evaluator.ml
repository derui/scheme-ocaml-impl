(** This module provides evaluation process by context. *)

module T = Type
module C = Continuation
module E = Environment

type result =
  [ `Value       of T.data
  | `Expand      of T.data
  | `Cont        of T.data
  | `Call_syntax of T.special_form * T.data
  ]

module type S = sig
  val eval : T.env -> T.data -> result T.evaluation_result
end

module Step_evaluator : S = struct
  let is_primitive = function T.Number _ | True | False | Empty_list | Scheme_string _ -> true | _ -> false

  let eval_symbol env sym =
    match E.get env ~key:sym with None -> T.raise_error @@ Printf.sprintf "%s is not bound" sym | Some v -> Ok v

  let eval_primitive v = Ok v

  (* evaluate a nested expression of expression *)
  let eval env expr =
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
