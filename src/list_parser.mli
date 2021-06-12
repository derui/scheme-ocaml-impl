type 'a t = Type.data -> ('a * Type.data, Type.scheme_error) result

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f t] apply [f] to [t]. *)

val pure : 'a -> 'a t
(** [pure v] return a new [t] with [v]. *)

val apply : ('b -> 'c) t -> 'b t -> 'c t
(** [apply ft t] apply function in applicative to other applicative *)

val bind : 'a t -> ('a -> 'b t) -> 'b t
(** [bind t f] bind for monad *)

(** Infix operators *)
module Infix : sig
  val ( <*> ) : ('b -> 'c) t -> 'b t -> 'c t

  val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
end

(** Let syntax *)
module Let_syntax : sig
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

  val ( let+ ) : ('b -> 'c) t -> 'b t -> 'c t
end

val ( *> ) : 'a t -> 'b t -> 'b t
(** [v1 *> v2] this operator select right hand argument. *)

val ( *< ) : 'a t -> 'b t -> 'a t
(** [v1 *< v2] this operator select left hand argument. *)

val element : Type.data t
(** [element] get current element that parser point *)

val cdr : Type.data t
(** [cdr] get rest of list. *)

val zero : 'a t
(** [zero] returns terminator of parser *)

val choice : 'a t -> 'a t -> 'a t
(** [choice a b] choice a parser what it is not terminator. *)

val ( <|> ) : 'a t -> 'a t -> 'a t
(** [a <|> b] infix operator of [choice] *)

val tap : (Type.data -> 'a) -> unit t
(** [tap f parser] install tapping function [f] into [parser]. *)

val satisfy : (Type.data -> bool) -> Type.data t
(** [satisfy f] make satisfaction parser with [f]. *)

val many : 'a t -> 'a list t
(** [many v] get parser with zero or many list of [v] *)

val many1 : 'a t -> 'a list t
(** [many v] get parser what return list having least one element as [v] *)

val chainl1 : 'a t -> ('a -> 'a -> 'a) t -> 'a t
(** [chainl1 v f] convert two elements into one element. *)

val chainl : 'a t -> ('a -> 'a -> 'a) t -> 'a -> 'a t
(** [chainl v f] convert two elements into one element. *)
