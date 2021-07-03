val cond_expand : Cond_expand.t List_parser.t
(** [cond_expand] return a parser that is able to use within other List_parser combinator *)

val parse : Type.data -> (Cond_expand.t, Type.scheme_error) result
(** [parse data] parse a data as Cond_expand.t *)
