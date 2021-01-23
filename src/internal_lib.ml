module S = Syntax

let length_of_list arg =
  let rec length' accum = function
    | S.Empty_list     -> accum
    | S.Cons (_, rest) -> length' (succ accum) rest
    | _                -> failwith "defined for only proper list"
  in
  length' 0 arg

let validate_arguments formal data =
  match formal with
  | S.Any _                   ->
      if S.Data.is_proper_list data then Ok data
      else S.raise_error @@ Printf.sprintf "%s is not proper list" @@ S.Data.to_string data
  | S.Fixed args              ->
      let len = List.length args in
      let arg_len = length_of_list data in
      if len <> arg_len then S.raise_error @@ Printf.sprintf "procedure requires %d, but got %d" len arg_len
      else Ok data
  | S.Fixed_and_any (args, _) ->
      let len = List.length args in
      let arg_len = length_of_list data in
      if len > arg_len then S.raise_error @@ Printf.sprintf "procedure requires at least %d arguments" len else Ok data

let list_to_scheme_list list =
  let rec to_list' accum = function [] -> accum | v :: rest -> to_list' (S.Cons (v, accum)) rest in
  List.rev list |> to_list' S.Empty_list
