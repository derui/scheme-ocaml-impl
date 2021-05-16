type 'a next_state =
  [ `Finished        of 'a
  | `Continue        of Type.data
  | `Popped          of Type.data
  | `End_instruction
  ]

let next_state_show = function
  | `Finished _      -> "Finished"
  | `Continue _      -> "Continue"
  | `Popped _        -> "Popped"
  | `End_instruction -> "End_instruction"

let next_state_pp fmt t = Format.fprintf fmt "%s" @@ next_state_show t

module type Status = sig
  type t

  val clone : t -> t

  val execution_pointer : t -> Execution_pointer.t

  val set_execution_pointer : Execution_pointer.t -> t -> unit
end

module type Instance = sig
  type status

  type context

  val instance : context

  val next : context -> status next_state

  val update_status : context -> f:(status -> status) -> unit

  val push_continuation : context -> status -> unit

  val pop_continuation : context -> Type.data -> unit

  val status : context -> status
end
