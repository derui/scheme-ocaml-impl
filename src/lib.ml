(** basic library *)

module Result = struct
  include Stdlib.Result

  let bind v f = match v with Ok v -> f v | Error e -> Error e

  let ( >>= ) = bind

  module Let_syntax = struct
    let ( let* ) = bind

    let ( and* ) = bind
  end
end
