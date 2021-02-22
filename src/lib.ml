(** basic library *)

module Result = struct
  include Stdlib.Result

  let bind v f = match v with Ok v -> f v | Error e -> Error e

  module Infix = struct
    let ( >>= ) = bind

    let ( >>| ) v f = Result.map f v
  end

  module Let_syntax = struct
    let ( let* ) = bind

    let ( and* ) = bind
  end
end

module Option = struct
  include Stdlib.Option

  let bind v f = match v with Some v -> f v | None -> None

  module Infix = struct
    let ( >>= ) = bind

    let ( >>| ) v f = Option.map f v
  end

  module Let_syntax = struct
    let ( let* ) = bind

    let ( and* ) = bind
  end
end
