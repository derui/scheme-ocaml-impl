module S = Syntax

let length_of_list arg =
  let rec length' accum = function
    | S.Empty_list     -> accum
    | S.Cons (_, rest) -> length' (succ accum) rest
    | _                -> failwith "defined for only proper list"
  in
  length' 0 arg
