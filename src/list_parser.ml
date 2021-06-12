module T = Type
module Pr = Printer

(* The type of parser *)
type 'a t = T.data -> ('a * T.data, T.scheme_error) result

let map : ('a -> 'b) -> 'a t -> 'b t =
 fun f p data -> match p data with Error _ as v -> v | Ok (v, rest) -> Ok (f v, rest)

let pure : 'a -> 'a t = fun v data -> Ok (v, data)

let apply fp xp data = match fp data with Error _ as v -> v | Ok (f, rest) -> (map f xp) rest

let bind : 'a t -> ('a -> 'b t) -> 'b t =
 fun v f data -> match v data with Error _ as e -> e | Ok (v, rest) -> f v rest

module Infix = struct
  let ( <*> ) = apply

  let ( <$> ) = map

  let ( >>= ) = bind
end

module Let_syntax = struct
  let ( let* ) = bind

  let ( let+ ) = apply
end

open Let_syntax
open Infix

(* Apply [p1] and [p2] sequentially and use right result *)
let ( *> ) p p2 = Infix.((fun _ y -> y) <$> p <*> p2)

(* Apply [p1] and [p2] sequentially and use left result *)
let ( *< ) p p2 = Infix.((fun x _ -> x) <$> p <*> p2)

let element = function
  | T.Empty_list                           -> T.raise_syntax_error "end of list"
  | Cons { car = v; cdr = Cons _ as rest } -> Ok (v, rest)
  | Cons { car = v; cdr = T.Empty_list }   -> Ok (v, Empty_list)
  | Cons { car = v; cdr = k }              -> Ok (v, k)
  | _ as v                                 -> T.raise_syntax_error (Printf.sprintf "malformed list: %s" @@ Pr.print v)

let cdr = function
  | T.Empty_list -> T.raise_syntax_error "should be end"
  | Cons _       -> T.raise_syntax_error "not malformed list"
  | v            -> Ok (v, T.Empty_list)

let zero v = T.raise_syntax_error (Printf.sprintf "empty: %s" @@ Pr.print v)

let choice p q data =
  let p = p data in
  let q = q data in
  match (p, q) with Error _, Error _ -> T.raise_syntax_error "can not choice" | Error _, Ok v | Ok v, _ -> Ok v

(* combinator to choice *)
let ( <|> ) = choice

let tap f data =
  let p =
    let* v = element in
    f v |> pure
  in
  p data |> ignore;
  Ok ((), data)

let satisfy p =
  let* v = element in
  if p v then pure v else zero

let many : 'a t -> 'a list t =
 fun p ->
  let p = (fun v -> [ v ]) <$> p in
  let rec many' accum =
    let* v = p <|> pure [] in
    match v with [] -> List.rev accum |> pure | v :: _ -> many' (v :: accum)
  in
  many' [] <|> pure []

let many1 p =
  let* p' = p in
  let* ps = many p in
  fun data_list -> Ok (p' :: ps, data_list)

(* chain one or more repeated operator to result of parser. *)
let chainl1 : 'a t -> ('a -> 'a -> 'a) t -> 'a t =
 fun p op ->
  let rec chain_rest a =
    let result_of_cycle =
      let* f = op in
      let* v' = p in
      chain_rest (f a v')
    in
    result_of_cycle <|> pure a
  in
  let* a = p in
  chain_rest a

(* chain zero or more repeated operator to result of parser. *)
let chainl p op a = chainl1 p op <|> pure a
