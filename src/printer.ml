module S = Syntax

type t = S.data

let rec to_string = function
  | S.Symbol s      -> s
  | Number s        -> s
  | Cons _ as v     -> cons_to_string [] v
  | True            -> "#t"
  | False           -> "#f"
  | Empty_list      -> "()"
  | Primitive_fun _ -> "<#proc ...>"
  | Closure _       -> "<#closure ...>"
  | Syntax_fun _    -> "<#syntax ...>"

and cons_to_string accum = function
  | Cons (v, (Cons _ as rest)) -> cons_to_string (v :: accum) rest
  | Cons (v, Empty_list)       -> List.map to_string (v :: accum)
                                  |> List.rev |> String.concat " " |> Printf.sprintf "(%s)"
  | Cons (v, rest)             ->
      let accum = List.map to_string (v :: accum) |> List.rev |> String.concat " " in
      let rest = to_string rest in
      Printf.sprintf "(%s . %s)" accum rest
  | _                          -> failwith "Invalid syntax"

let print = to_string

let show = to_string

let pp fmt v = Format.fprintf fmt "%s" @@ show v