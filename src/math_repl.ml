module S = Syntax
module E = Environment

let print = function
  | Error e -> Printf.printf "Error occurred:\n    %s" e
  | Ok v    -> Printf.printf "%s\n" @@ Syntax.Data.to_string v

let initialize_global_env env =
  Environment.set env ~key:"+" ~v:(S.Value (S.Native_fun Buildin_op.Number_op.Export.plus)) |> ignore

let rec run () =
  let env = Environment.make [] in
  initialize_global_env env;
  print_string "repl> ";
  let command = read_line () in
  match command with
  | "halt" -> ()
  | _      -> (
      try
        let terms = Lexing.from_string command |> Parser.program Lexer.token in
        List.iter (fun term -> Eval.eval env term |> print) terms |> run
      with Parser.Error ->
        Printexc.print_backtrace stdout;
        run () )
