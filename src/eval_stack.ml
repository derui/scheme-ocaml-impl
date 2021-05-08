(** This module provides a context for evaluation. *)

module T = Type

module Evaluated_value_map = Map.Make (struct
  type t = int

  let compare = Stdlib.compare
end)

type common = {
  expression : T.data;
  rest_expression : T.data option;
}

type kind =
  | In_closure
  | In_expression

type t =
  | Expression of {
      mutable common : common;
      mutable evaluated_stack : T.data;
    }
  | Closure    of {
      mutable common : common;
      mutable evaluated_value : T.data;
    }

let make ~kind expr =
  assert (T.is_proper_list expr);
  let rest_expression = match expr with T.Cons _ -> Some expr | _ -> None in
  match kind with
  | In_closure    -> Closure { common = { expression = expr; rest_expression }; evaluated_value = T.Empty_list }
  | In_expression -> Expression { common = { expression = expr; rest_expression }; evaluated_stack = T.Empty_list }

let kind = function Closure _ -> In_closure | Expression _ -> In_expression

let expression = function
  | Closure { common = { expression; _ }; _ } -> expression
  | Expression { common = { expression; _ }; _ } -> expression

let clone = function
  | Closure { common; evaluated_value }    -> Closure { common; evaluated_value }
  | Expression { common; evaluated_stack } -> Expression { common; evaluated_stack }

let current t =
  let t = match t with Closure { common; _ } -> common | Expression { common; _ } -> common in
  match t.rest_expression with None -> None | Some expr -> ( match expr with T.Cons (v, _) -> Some v | _ -> None)

let replace_current t expr =
  let common = match t with Closure { common; _ } -> common | Expression { common; _ } -> common in
  let rest_expression =
    match common.rest_expression with
    | None   -> None
    | Some e -> ( match e with T.Cons (_, rest) -> Some (T.Cons (expr, rest)) | _ -> None)
  in

  let () =
    match t with
    | Closure t    -> t.common <- { common with rest_expression }
    | Expression t -> t.common <- { common with rest_expression }
  in
  t

let push_value t ~value =
  let () =
    match t with
    | Expression t ->
        t.evaluated_stack <- T.Cons (value, t.evaluated_stack);
        Option.iter
          (fun expr ->
            match expr with
            | T.Cons (_, (T.Cons _ as rest)) -> t.common <- { t.common with rest_expression = Some rest }
            | T.Cons (_, T.Empty_list)       -> t.common <- { t.common with rest_expression = None }
            | _                              -> ())
          t.common.rest_expression
    | Closure t    ->
        t.evaluated_value <- value;
        Option.iter
          (fun expr ->
            match expr with
            | T.Cons (_, (T.Cons _ as rest)) -> t.common <- { t.common with rest_expression = Some rest }
            | T.Cons (_, T.Empty_list)       -> t.common <- { t.common with rest_expression = None }
            | _                              -> ())
          t.common.rest_expression
  in
  t

(** [evaluated_values t] makes a list that contains value is same order as evaluated *)
let evaluated_values = function
  | Expression { evaluated_stack; _ } -> evaluated_stack
  | Closure { evaluated_value; _ }    -> evaluated_value
