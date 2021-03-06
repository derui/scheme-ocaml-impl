(** This module provides subset of standard operations. *)

module T = Type
module D = Data_type

module List_op = struct
  let car _ arg =
    match arg with T.Cons { car = Cons { car = v; _ }; _ } -> Ok v | _ -> T.raise_error "pair requirement"

  let cdr _ arg =
    match arg with T.Cons { car = Cons { cdr = v; _ }; _ } -> Ok v | _ -> T.raise_error "pair requirement"

  let set_car _ arg =
    match arg with
    | T.Cons { car = Cons cell; cdr = Cons { car = v; _ } } ->
        cell.car <- v;
        Ok T.Undef
    | _ -> T.raise_error "pair requirement"

  let set_cdr _ arg =
    match arg with
    | T.Cons { car = Cons cell; cdr = Cons { car = v; _ } } ->
        cell.cdr <- v;
        Ok T.Undef
    | _ -> T.raise_error "pair requirement"

  let cons _ arg =
    match arg with
    | T.Cons { car = v; cdr = T.Cons { car = v2; _ } } -> Ok (T.Cons { car = v; cdr = v2 })
    | _ -> T.raise_error "two argument requirement"

  let is_pair _ arg = match arg with T.Cons _ | T.Empty_list -> Ok T.True | _ -> Ok T.False

  let length cont arg =
    let open Lib.Result.Let_syntax in
    match arg with
    | T.Cons { car = T.Empty_list; cdr = T.Empty_list } -> T.Number "0" |> Result.ok
    | _ ->
        let* arg = car cont arg in
        let len = Internal_lib.length_of_list arg in
        T.Number (string_of_int len) |> Result.ok

  let reverse _ arg = Internal_lib.reverse arg

  let append _ arg =
    let open Lib.Result.Let_syntax in
    let list, _ = Internal_lib.scheme_list_to_list arg in
    let list = list |> List.filter (function T.Empty_list -> false | _ -> true) in
    match list with
    | []    -> Ok T.Empty_list
    | [ v ] -> Ok v
    | _     ->
        let rec append' accum = function [] -> accum | v :: rest -> append' (v :: accum) rest in
        let rec append_list accum = function
          | [ v ] -> Ok (accum, v)
          | (T.Cons _ as v) :: rest when T.is_proper_list v ->
              let accum = Internal_lib.scheme_list_to_list v |> fst |> append' accum in
              append_list accum rest
          | _ -> T.raise_error "invalid path"
        in
        let rec list_to_cons accum list =
          let cons car cdr = T.(cons car cdr) in
          match list with [] -> accum | car :: rest -> list_to_cons (cons car accum) rest
        in
        let* accum, v = append_list [] list in
        Ok (list_to_cons v accum)

  module Export = struct
    let length = (D.Argument_formal.Fixed [ "list" ], length)

    let car = (D.Argument_formal.Fixed [ "list" ], car)

    let cdr = (D.Argument_formal.Fixed [ "list" ], cdr)

    let reverse = (D.Argument_formal.Fixed [ "list" ], reverse)

    let cons = (D.Argument_formal.Fixed [ "v1"; "v2" ], cons)

    let append = (D.Argument_formal.Any "list", append)
  end
end

module Number_op = struct
  let plus _ args =
    let open Lib.Result.Let_syntax in
    let rec plus' accum v =
      match v with
      | T.Empty_list                        -> Ok accum
      | Cons { car = Number v; cdr = rest } ->
          let v = int_of_string v in
          plus' (accum + v) rest
      | _                                   -> T.raise_error
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
