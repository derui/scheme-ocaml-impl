(** This module provides a evaluation stack to hold intermediate values for procedure. *)

module T = Type

module Evaluated_value_map = Map.Make (struct
  type t = int

  let compare = Stdlib.compare
end)

type t = { mutable evaluated_values : T.data }

let make ?(evaluated_values = T.Empty_list) () = { evaluated_values }

let clone t = { evaluated_values = t.evaluated_values }

let push_value t ~value =
  let () = t.evaluated_values <- T.Cons (value, t.evaluated_values) in
  t

(** [evaluated_values t] makes a list that contains value is same order as evaluated *)
let evaluated_values { evaluated_values } = evaluated_values
