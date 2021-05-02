(** This module provides a context for evaluation. *)

module T = Type

module Evaluated_value_map = Map.Make (struct
  type t = int

  let compare = Stdlib.compare
end)

type t = {
  whole_expression : T.External_representation.t;
  evaluated_map : T.data Evaluated_value_map.t;
  evaluated_loc : int;
}

let make expr = { whole_expression = expr; evaluated_map = Evaluated_value_map.empty; evaluated_loc = 0 }

let whole_expression t = t.whole_expression

(** [forward t] force forward evaluated location *)
let forward t = { t with evaluated_loc = succ t.evaluated_loc }

let save_value t value =
  let loc = t.evaluated_loc in
  { t with evaluated_map = Evaluated_value_map.add loc value t.evaluated_map; evaluated_loc = succ loc }

(** [evaluated_values t] makes a list that contains value is same order as evaluated *)
let evaluated_values t =
  Evaluated_value_map.to_seq t.evaluated_map
  |> List.of_seq
  |> List.sort (fun (v1, _) (v2, _) -> Stdlib.compare v1 v2)
  |> List.map snd
