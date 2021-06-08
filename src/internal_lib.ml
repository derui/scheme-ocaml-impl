module T = Type
module D = Data_type

let length_of_list arg =
  let rec length' accum = function
    | T.Empty_list           -> accum
    | Cons { cdr = rest; _ } -> length' (succ accum) rest
    | _                      -> failwith "defined for only proper list"
  in
  length' 0 arg

let validate_arguments formal data =
  match formal with
  | D.Argument_formal.Any _ ->
      if T.is_proper_list data then Ok data
      else T.raise_error @@ Printf.sprintf "%s is not proper list" @@ Printer.print data
  | Fixed args              ->
      let len = List.length args in
      let arg_len = length_of_list data in
      if len <> arg_len then T.raise_error @@ Printf.sprintf "procedure requires %d, but got %d" len arg_len
      else Ok data
  | Fixed_and_any (args, _) ->
      let len = List.length args in
      let arg_len = length_of_list data in
      if len > arg_len then T.raise_error @@ Printf.sprintf "procedure requires at least %d arguments" len else Ok data

let list_to_scheme_list list =
  let rec to_list' accum = function [] -> accum | v :: rest -> to_list' (T.Cons { car = v; cdr = accum }) rest in
  List.rev list |> to_list' T.Empty_list

let scheme_list_to_list list =
  let rec to_list' accum = function
    | T.Empty_list                   -> (List.rev accum, None)
    | T.Cons { car = v; cdr = rest } -> to_list' (v :: accum) rest
    | _ as v                         -> (List.rev accum, Some v)
  in
  to_list' [] list

let take_list arg n =
  let rec loop accum count v =
    if count = 0 then List.rev accum
    else
      match v with
      | T.Cons { car = v; cdr = T.Empty_list } -> List.rev (v :: accum)
      | T.Cons { car = v; cdr = T.Cons _ as rest } -> loop (v :: accum) (pred count) rest
      | T.Cons { car = v; _ } -> List.rev (v :: accum)
      | _ -> []
  in
  loop [] n arg |> list_to_scheme_list

let tail_list arg n =
  let rec loop count v =
    if count = 0 then v
    else
      match v with
      | T.Cons { cdr = T.Cons _ as rest; _ } -> loop (pred count) rest
      | T.Cons _                             -> failwith "Length of list less than n"
      | _                                    -> failwith "Invalid argument"
  in
  loop n arg

let reverse arg =
  let rec reverse' accum = function
    | T.Empty_list                 -> Ok accum
    | Cons { car = v; cdr = rest } -> reverse' (T.Cons { car = v; cdr = accum }) rest
    | _                            -> T.raise_error @@ Printf.sprintf "%s is not proper list" @@ Printer.print arg
  in
  reverse' Empty_list arg
