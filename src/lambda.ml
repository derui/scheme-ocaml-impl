(** evaluate lambda function with environment (but environment support is not implemented) *)
let eval env (requirement, f) arg =
  match requirement with
  | None             -> f env arg
  | Some requirement ->
      let arg_len = Internal_lib.length_of_list arg in
      if arg_len <> requirement then
        Error (Printf.sprintf "invalid arguments: required %d, but %d given" requirement arg_len)
      else f env arg
