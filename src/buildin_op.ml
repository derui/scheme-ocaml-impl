(** This module provides subset of standard operations. *)

let is_number args = match List.hd args with Syntax.Number _ -> Syntax.True | _ -> Syntax.False

let is_number = (1, is_number)
