include module type of Runtime_intf

(** Make runtime module instance to manage libraries and features.

    The module returned from this functor is an instance, so you should use module returned like object. *)
module Make (Library_producer : Library_producer.S) (Feature_query : Feature_query.S) : S
