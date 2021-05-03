(** This module provides a context for evaluation. *)

module T = Type

module Evaluated_value_map = Map.Make (struct
  type t = int

  let compare = Stdlib.compare
end)

type t = {
  expression : T.data;
  mutable rest_expression : T.data option;
  mutable evaluated_stack : T.data list;
}

let make expr =
  assert (T.is_proper_list expr);
  let rest_expression = match expr with T.Cons _ -> Some expr | _ -> None in
  { expression = expr; rest_expression; evaluated_stack = [] }

let clone t =
  {
    expression = t.expression;
    rest_expression = t.rest_expression;
    evaluated_stack = List.map (fun v -> v) t.evaluated_stack;
  }

let current t =
  match t.rest_expression with None -> None | Some expr -> ( match expr with T.Cons (v, _) -> Some v | _ -> None )

let push_value t ~value =
  t.evaluated_stack <- value :: t.evaluated_stack;
  Option.iter
    (fun expr ->
      match expr with
      | T.Cons (_, (T.Cons _ as rest)) -> t.rest_expression <- Some rest
      | T.Cons (_, T.Empty_list)       -> t.rest_expression <- None
      | _                              -> ())
    t.rest_expression;
  t

(** [evaluated_values t] makes a list that contains value is same order as evaluated *)
let evaluated_values t = t.evaluated_stack |> List.rev
