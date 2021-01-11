let is_math_symbol = function "*" | "+" | "-" | "/" -> true | _ -> false

let arg_to_number args index =
  match List.nth_opt args index with
  | Some (Syntax.Number num) -> int_of_string num
  | _                        -> failwith "Can not handle argument"

let rec eval_combo sym args =
  let eval_micro index =
    match List.nth args index with
    | Syntax.Number _                         -> arg_to_number args index
    | Syntax.List (Syntax.Symbol sym :: rest) -> eval_combo sym rest
    | _                                       -> failwith "Can not handle list"
  in
  let v1 = eval_micro 0 and v2 = eval_micro 1 in
  match sym with
  | "+" -> v1 + v2
  | "-" -> v1 - v2
  | "*" -> v1 * v2
  | "/" -> v1 / v2
  | _   -> failwith @@ Printf.sprintf "Unknown symbol: %s" sym

let math_eval = function
  | [] -> ()
  | Syntax.List (Syntax.Symbol sym :: rest) :: _ when is_math_symbol sym ->
      if List.length rest <> 2 then print_string "Allow only 2 arguments"
      else
        let ret = eval_combo sym rest in
        Printf.printf "%d\n" ret
  | _ -> ()

let rec run () =
  print_string "repl> ";
  let command = read_line () in
  match command with
  | "halt" -> ()
  | _      -> (
      try
        let terms = Lexing.from_string command |> Parser.program Lexer.token in
        math_eval terms |> run
      with Parser.Error ->
        Printexc.print_backtrace stdout;
        run () )
