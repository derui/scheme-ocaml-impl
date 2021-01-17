module S = Syntax
(** This module provides subset of standard operations. *)

module List_op = struct
  let car arg = match arg with S.Cons (S.Cons (v, _), _) -> v | _ -> failwith "pair requirement"

  let cdr arg = match arg with S.Cons (S.Cons (_, v), _) -> v | _ -> failwith "pair requirement"

  let length arg =
    let arg = car arg in
    let len = Internal_lib.length_of_list arg in
    S.Number (string_of_int len)

  module Export = struct
    let length = (Some 1, length)

    let car = (Some 1, car)

    let cdr = (Some 1, cdr)
  end
end

module Number_op = struct
  let plus args =
    let rec plus' accum v =
      match v with
      | S.Empty_list              -> accum
      | S.Cons (S.Number v, rest) ->
          let v = int_of_string v in
          plus' (accum + v) rest
      | _                         -> failwith
                                     @@ Printf.sprintf "'+ is not implement for that expr is not number %s"
                                     @@ S.Data.to_string v
    in
    let result = plus' 0 args in
    S.Number (string_of_int result)

  module Export = struct
    let plus = (None, plus)
  end
end

let is_number args = match args with S.Number _ -> S.True | _ -> S.False

let is_number = (1, is_number) [@@warning "-32"]
