open Ocaml_scheme
module T = Type
module E = Environment

let print = function
  | Error (T.Error_obj { message; irritants; _ }) ->
      let irritants = " " ^ (List.map Printer.print irritants |> String.concat " ") in
      Printf.printf "Error occurred:\n    %s%s\n" message irritants
  | Error (T.Syntax_error { message; args }) ->
      let irritants = " " ^ (List.map Printer.print args |> String.concat " ") in
      Printf.printf "Error occurred:\n    %s%s\n" message irritants
  | Ok v -> Printf.printf "%s\n" @@ Printer.print v

let initialize_global_env env =
  Environment.set env ~key:"+" ~v:(T.Value (T.Primitive_fun Primitive_op.Number_op.Export.plus)) |> ignore;
  Environment.set env ~key:"define" ~v:(T.Special_form Special_form.Export.eval_define) |> ignore;
  Environment.set env ~key:"if" ~v:(T.Special_form Special_form.Export.eval_if) |> ignore;
  Environment.set env ~key:"set!" ~v:(T.Special_form Special_form.Export.eval_set_force) |> ignore;
  Environment.set env ~key:"lambda" ~v:(T.Special_form Special_form.Export.eval_lambda) |> ignore;
  Environment.set env ~key:"quote" ~v:(T.Special_form Special_form.Export.eval_quote) |> ignore;
  Environment.set env ~key:"unquote" ~v:(T.Special_form Special_form.Export.eval_unquote) |> ignore;
  Environment.set env ~key:"quasiquote" ~v:(T.Special_form Special_form.Export.eval_quasiquote) |> ignore;
  Environment.set env ~key:"define-syntax" ~v:(T.Special_form Syntax_transformer.eval_define_syntax) |> ignore;
  Environment.set env ~key:"let-syntax" ~v:(T.Special_form Syntax_transformer.eval_let_syntax) |> ignore;
  Environment.set env ~key:"syntax-rules" ~v:(T.Special_form (fun _ data -> Syntax_transformer.eval_syntax_rules data))
  |> ignore;
  let chan = open_in "./scheme/r7rs/base-expressions.scm" in
  let () =
    try Lexing.from_channel chan |> Parser.program Lexer.token |> List.map (Eval.eval env) |> ignore
    with _ as e -> raise e
  in
  close_in chan

let run () =
  let env = Environment.make [] in
  initialize_global_env env;
  let rec run' env () =
    let open Lib.Result.Infix in
    print_string "repl> ";
    try
      let v = Read.read env T.Empty_list >>= Eval.eval env in
      print v |> run' env
    with Parser.Error ->
      Printexc.print_backtrace stdout;
      run' env ()
  in
  run' env ()

let () = run ()
