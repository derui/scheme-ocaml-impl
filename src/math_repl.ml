module S = Syntax
module E = Environment

let print = function
  | Error e -> Printf.printf "Error occurred:\n    %s\n" e
  | Ok v    -> Printf.printf "%s\n" @@ Syntax.Data.to_string v

let initialize_global_env env =
  Environment.set env ~key:"+" ~v:(S.Value (S.Native_fun Buildin_op.Number_op.Export.plus)) |> ignore;
  Environment.set env ~key:"define" ~v:(S.Special_form Special_form.Export.eval_define) |> ignore;
  Environment.set env ~key:"if" ~v:(S.Special_form Special_form.Export.eval_if) |> ignore;
  Environment.set env ~key:"set!" ~v:(S.Special_form Special_form.Export.eval_set_force) |> ignore;
  Environment.set env ~key:"let" ~v:(S.Special_form Special_form.Export.eval_let) |> ignore

let run () =
  let env = Environment.make [] in
  initialize_global_env env;
  let rec run' env () =
    let open Lib.Result in
    print_string "repl> ";
    try
      let v = Read.read env S.Empty_list >>= Eval.eval env in
      print v |> run' env
    with Parser.Error ->
      Printexc.print_backtrace stdout;
      run' env ()
  in
  run' env ()
