type 'a t

val make : ?parent_env:'a t -> (string * 'a) list -> 'a t
(** make a new environment with bindings and [parent_env] if exists. *)

val set : 'a t -> key:string -> v:'a -> unit
(** [set t ~key ~v] set [v] to [key]. This function behave to replace current value of [key]. *)

val replace : 'a t -> key:string -> v:'a -> unit option
(** [replace t ~key ~v] set [v] to [key]. This function behave to replace current value of [key]. If [key] is not
    defined in whole environment, return [None]. *)

val get : 'a t -> key:string -> 'a option
(** [get t ~key] get the binding that bounded by [key]. If [key] is not defined in whole environment, return None. *)
