open Type

type t = {
  env : binding Environment.t;
  symbol : string;
  body : data;
}
(** continuation is almost same as closure, but do not have argument formal. *)

let make ~body ~parent_env ~symbol = { env = parent_env; symbol; body }

let apply t value evaluator =
  let env = Environment.make ~parent_env:t.env [ (t.symbol, Value value) ] in
  evaluator env t.body
