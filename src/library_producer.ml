(** The module to produce library and list of it *)
module type S = sig
  val all_list : unit -> Library.t list
  (** [all_list ()] get all list of library that is loaded or is not loaded *)

  val produce : Library.name -> Library.t option
  (** [produce name] get the library having [name]. If it was already loaded, return loaded library. If no any library
      that have name found, return None. *)
end
