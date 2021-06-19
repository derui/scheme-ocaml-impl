module T = Type

(** This signature defines interface of runtime *)
module type S = sig
  val get_library : Library.name -> Library.t option
  (** [get_library name] get a library from runtime. If the library was not found or could not load, return [None]. *)

  val define_library : Library.t -> unit
  (** [define_library library] add a library to runtime on demand. *)

  val is_requirement_filled : Feature_query.Feature_requirement.t -> bool
  (** [is_requirement_filled requirement] return [requirement] is implemented *)
end
