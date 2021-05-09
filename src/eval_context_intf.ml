module type Status = sig
  type t

  val clone : t -> t
end

module type Instance = sig
  type status

  type context

  val instance : context

  val next : context -> [> `Finished of status | `Continue ]

  val update_status : context -> f:(status -> status) -> unit

  val push_continuation : context -> status -> unit

  val pop_continuation : context -> unit

  val status : context -> status
end
