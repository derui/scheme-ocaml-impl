open Ocaml_scheme
module T = Type
module E = Environment

let print = function
  | Error e -> Printf.printf "Error occurred:\n    %s\n" e
  | Ok v    -> Printf.printf "%s\n" @@ Printer.print v

let initialize_global_env env =
  Environment.set env ~key:"+" ~v:(T.Value (T.Primitive_fun Primitive_op.Number_op.Export.plus)) |> ignore;
  Environment.set env ~key:"define" ~v:(T.Special_form Special_form.Export.eval_define) |> ignore;
  Environment.set env ~key:"if" ~v:(T.Special_form Special_form.Export.eval_if) |> ignore;
  Environment.set env ~key:"set!" ~v:(T.Special_form Special_form.Export.eval_set_force) |> ignore;
  Environment.set env ~key:"lambda" ~v:(T.Special_form Special_form.Export.eval_lambda) |> ignore;
  Environment.set env ~key:"let" ~v:(T.Special_form Special_form.Export.eval_let) |> ignore;
  Environment.set env ~key:"quote" ~v:(T.Special_form Special_form.Export.eval_quote) |> ignore;
  Environment.set env ~key:"unquote" ~v:(T.Special_form Special_form.Export.eval_unquote) |> ignore;
  Environment.set env ~key:"quasiquote" ~v:(T.Special_form Special_form.Export.eval_quasiquote) |> ignore

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
