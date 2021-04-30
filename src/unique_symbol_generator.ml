(** this module provides unique symbol generator in same instance *)

module type Instance = sig
  val next : unit -> string
end

let make () =
  ( module struct
    let current_val = ref 0

    let next () =
      let current = !current_val in
      incr current_val;
      Printf.sprintf ";%d" current
  end : Instance )

let next instance =
  let module I = (val instance : Instance) in
  I.next ()
