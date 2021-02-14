open Type

val read : binding Environment.t -> data -> data evaluation_result
(** [read env data] read a term from stdin or port. [Notice] read from port is not implemented, and did not implement
    end-of-file object, so on *)
