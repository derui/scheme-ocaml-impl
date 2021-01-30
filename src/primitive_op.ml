module S = Syntax
(** This module provides subset of standard operations. *)

module List_op = struct
  let car arg = match arg with S.Cons (S.Cons (v, _), _) -> Ok v | _ -> S.raise_error "pair requirement"

  let cdr arg = match arg with S.Cons (S.Cons (_, v), _) -> Ok v | _ -> S.raise_error "pair requirement"

  let length arg =
    let open Lib.Result.Let_syntax in
    let* arg = car arg in
    let len = Internal_lib.length_of_list arg in
    S.Number (string_of_int len) |> Result.ok

  let reverse arg =
    let rec reverse' accum = function
      | S.Empty_list     -> Ok accum
      | S.Cons (v, rest) -> reverse' (S.Cons (v, accum)) rest
      | _                -> S.raise_error @@ Printf.sprintf "%s is not proper list" @@ Printer.print arg
    in
    reverse' S.Empty_list arg

  module Export = struct
    let length = (S.Fixed [ "list" ], length)

    let car = (S.Fixed [ "list" ], car)

    let cdr = (S.Fixed [ "list" ], cdr)

    let reverse = (S.Fixed [ "list" ], reverse)
  end
end

module Number_op = struct
  let plus args =
    let open Lib.Result.Let_syntax in
    let rec plus' accum v =
      match v with
      | S.Empty_list              -> Ok accum
      | S.Cons (S.Number v, rest) ->
          let v = int_of_string v in
          plus' (accum + v) rest
      | _                         -> S.raise_error
                                     @@ Printf.sprintf "'+ is not implement for that expr is not number %s"
                                     @@ Printer.print v
    in
    let* result = plus' 0 args in
    S.Number (string_of_int result) |> Result.ok

  module Export = struct
    let plus = (S.Any "numbers", plus)
  end
end

let is_number args = match args with S.Number _ -> S.True | _ -> S.False

let is_number = (1, is_number) [@@warning "-32"]
