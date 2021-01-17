module S = Syntax

let is_math_symbol = function "*" | "+" | "-" | "/" -> true | _ -> false

module Env = struct
  type t = Syntax.env

  let tl_env = ref []

  let set key v = tl_env := (key, v) :: !tl_env

  let get key = List.assoc_opt key !tl_env

  let reset () = tl_env := []
end

let eval_symbol sym =
  match Env.get sym with
  | None   -> Result.error @@ Printf.sprintf "symbol %s is not defined in environment" sym
  | Some v -> Ok v

let is_primitive = function S.Number _ | S.True | S.False -> true | _ -> false

let eval_primitive v = Ok v

let is_special_form = function
  | S.Cons (S.Symbol sym, _) -> ( match sym with "define" -> true | _ -> false )
  | _                        -> false

let rec eval = function
  | Syntax.Symbol sym -> eval_symbol sym
  | _ as v when is_primitive v -> eval_primitive v
  | _ as v when is_special_form v -> eval_special_form v
  | Syntax.Cons (Syntax.Symbol sym, cdr) -> eval_apply sym @@ eval_list cdr
  | _ as v -> Error (Printf.sprintf "Can not handle expression now... %s\n" @@ Syntax.Data.to_string v)

and eval_special_form = function
  | S.Cons (S.Symbol name, S.Cons (S.Symbol sym, v)) when name = "define" -> eval_define sym v
  | _ as v -> Error (Printf.sprintf "Can not handle special form: %s" @@ S.Data.to_string v)

and eval_define sym v =
  let open Lib.Result.Let_syntax in
  let* value = eval v in
  Env.set sym value;
  Result.ok value

and eval_list v =
  let open Lib.Result.Let_syntax in
  let rec parse_list accum = function
    | Syntax.Empty_list      -> S.Empty_list :: accum |> Result.ok
    | Syntax.Cons (car, cdr) ->
        let* car = eval car in
        parse_list (car :: accum) cdr
    | _ as v                 ->
        let* car = eval v in
        car :: accum |> Result.ok
  in
  let* list = parse_list [] v in
  match list with
  | []        -> Ok S.Empty_list
  | v :: rest -> List.fold_left (fun list v -> S.Cons (v, list)) v rest |> Result.ok

and eval_apply symbol arg =
  let open Lib.Result.Let_syntax in
  let* defined_fun =
    let* v = eval_symbol symbol in
    if Syntax.Data.is_applicable v then Ok v else Result.error @@ Printf.sprintf "%s is not applicable" symbol
  in
  let* arg = arg in
  match defined_fun with Syntax.Native_fun f -> Lambda.eval f arg | _ -> failwith "Illegal path"

let print = function
  | Error e -> Printf.printf "Error occurred:\n    %s" e
  | Ok v    -> Printf.printf "%s\n" @@ Syntax.Data.to_string v

let initialize_global_env () = Env.set "+" (S.Native_fun Buildin_op.Number_op.Export.plus)

let rec run () =
  initialize_global_env ();
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
