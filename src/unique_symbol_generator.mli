(** module signature of Instance *)
module type Instance = sig
  val next : unit -> string
end

val make : unit -> (module Instance)
(** [make ()] create a new instance of generator. Each generators does not affect other generators. *)

val next : (module Instance) -> string
(** [next instance] get a new symbol name from generator. *)
