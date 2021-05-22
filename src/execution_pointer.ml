(** This module provides a context for evaluation. *)

module T = Type

type t = {
  expression : T.data;
  rest_expression : T.data option;
}

let make expr =
  assert (match expr with T.Cons _ -> true | _ -> false);
  { expression = expr; rest_expression = Some expr }

let clone t = { expression = t.expression; rest_expression = t.rest_expression }

let current t = match t.rest_expression with None -> None | Some (T.Cons (v, _)) -> Some v | Some v -> Some v

let next t =
  let rest =
    match t.rest_expression with
    | Some (T.Cons (_, Empty_list)) -> None
    | Some (T.Cons (_, rest))       -> Some rest
    | _                             -> None
  in
  { t with rest_expression = rest }

let replace_current t expr =
  let rest_expression =
    match t.rest_expression with Some (T.Cons (_, rest)) -> Some (T.Cons (expr, rest)) | _ -> None
  in
  { t with rest_expression }
