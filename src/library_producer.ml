(** The module to produce library and list of it *)
module type S = sig
  val exists : Library.name -> bool
  (** [exists name] return exists the library [name] or not *)

  val produce : Library.name -> Library.t option
  (** [produce name] get the library having [name]. If it was already loaded, return loaded library. If no any library
      that have name found, return None. *)
end
