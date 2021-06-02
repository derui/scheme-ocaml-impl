(** This module provides subset of standard operations. *)

module T = Type
module D = Data_type

module List_op = struct
  let car arg = match arg with T.Cons (Cons (v, _), _) -> Ok v | _ -> T.raise_error "pair requirement"

  let cdr arg = match arg with T.Cons (Cons (_, v), _) -> Ok v | _ -> T.raise_error "pair requirement"

  let cons arg =
    match arg with T.Cons (v, T.Cons (v2, _)) -> Ok (T.Cons (v, v2)) | _ -> T.raise_error "two argument requirement"

  let is_pair arg = match arg with T.Cons _ | T.Empty_list -> Ok T.True | _ -> Ok T.False

  let length arg =
    let open Lib.Result.Let_syntax in
    match arg with
    | T.Cons (T.Empty_list, T.Empty_list) -> T.Number "0" |> Result.ok
    | _                                   ->
        let* arg = car arg in
        let len = Internal_lib.length_of_list arg in
        T.Number (string_of_int len) |> Result.ok

  let reverse arg =
    let rec reverse' accum = function
      | T.Empty_list   -> Ok accum
      | Cons (v, rest) -> reverse' (T.Cons (v, accum)) rest
      | _              -> T.raise_error @@ Printf.sprintf "%s is not proper list" @@ Printer.print arg
    in
    reverse' Empty_list arg

  let append arg =
    let list, _ = Internal_lib.scheme_list_to_list arg in
    let list = list |> List.filter (function T.Empty_list -> false | _ -> true) in
    match list with
    | []    -> Ok T.Empty_list
    | [ v ] -> Ok v
    | _     ->
        let rec append' accum = function [] -> reverse accum | v :: rest -> append' (T.Cons (v, accum)) rest in
        append' T.Empty_list list

  module Export = struct
    let length = (D.Argument_formal.Fixed [ "list" ], length)

    let car = (D.Argument_formal.Fixed [ "list" ], car)

    let cdr = (D.Argument_formal.Fixed [ "list" ], cdr)

    let reverse = (D.Argument_formal.Fixed [ "list" ], reverse)

    let cons = (D.Argument_formal.Fixed [ "v1"; "v2" ], cons)
  end
end

module Number_op = struct
  let plus args =
    let open Lib.Result.Let_syntax in
    let rec plus' accum v =
      match v with
      | T.Empty_list          -> Ok accum
      | Cons (Number v, rest) ->
          let v = int_of_string v in
          plus' (accum + v) rest
      | _                     -> T.raise_error
                                 @@ Printf.sprintf "'+ is not implement for that expr is not number %s"
                                 @@ Printer.print v
    in
    let* result = plus' 0 args in
    T.Number (string_of_int result) |> Result.ok

  module Export = struct
    let plus = (D.Argument_formal.Any "numbers", plus)
  end
end

let is_number args = match args with T.Number _ -> T.True | _ -> False

let is_number = (1, is_number) [@@warning "-32"]
