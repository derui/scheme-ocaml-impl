(** evaluate lambda function with environment (but environment support is not implemented) *)
let eval (requirement, f) args =
  let arg_len = List.length args in
  if arg_len <> requirement then
    Error (Printf.sprintf "invalid arguments: required %d, but %d given" requirement arg_len)
  else Ok (f args)
