let is_math_symbol = function "*" | "+" | "-" | "/" -> true | _ -> false

module Env = struct
  type t = Syntax.env

  let tl_env = ref []

  let set key v = tl_env := (key, v) :: !tl_env

  let get key = List.assoc_opt key !tl_env
end

let eval_symbol sym =
  match Env.get sym with
  | None   -> Result.error @@ Printf.sprintf "symbol %s is not defined in environment" sym
  | Some v -> Ok v

let is_primitive = function Syntax.Number _ | Syntax.True | Syntax.False -> true | _ -> false

let eval_primitive v = Ok v

let rec eval = function
  | Syntax.Symbol sym -> eval_symbol sym
  | _ as v when is_primitive v -> eval_primitive v
  | Syntax.Cons (Syntax.Symbol sym, cdr) -> eval_apply sym @@ eval_list cdr
  | _ as v -> Error (Printf.sprintf "Can not handle expression now... %s\n" @@ Syntax.Data.to_string v)

and eval_list v =
  let open Lib.Result.Let_syntax in
  let rec parse_list accum = function
    | Syntax.Empty_list      -> List.rev accum |> Result.ok
    | Syntax.Cons (car, cdr) ->
        let* car = eval car in
        parse_list (car :: accum) cdr
    | _ as v                 ->
        let* car = eval v in
        List.rev (car :: accum) |> Result.ok
  in
  parse_list [] v

and eval_apply symbol cdr =
  let open Lib.Result.Let_syntax in
  let* defined_fun =
    let* v = eval_symbol symbol in
    if Syntax.Data.is_applicable v then Ok v else Result.error @@ Printf.sprintf "%s is not applicable" symbol
  in
  let* args = cdr in
  let* args =
    List.fold_left
      (fun args v ->
        let* args = args in
        let* v = eval v in
        Ok (v :: args))
      (Ok []) args
  in
  let* args = List.rev args |> Result.ok in
  match defined_fun with Syntax.Native_fun f -> Lambda.eval f args | _ -> failwith "Illegal path"

let print = function
  | Error e -> Printf.printf "Error occurred:\n    %s" e
  | Ok v    -> Printf.printf "%s\n" @@ Syntax.Data.to_string v

let rec run () =
  print_string "repl> ";
  let command = read_line () in
  match command with
  | "halt" -> ()
  | _      -> (
      try
        let terms = Lexing.from_string command |> Parser.program Lexer.token in
        List.iter (fun term -> eval term |> print) terms |> run
      with Parser.Error ->
        Printexc.print_backtrace stdout;
        run () )
