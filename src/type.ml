type argument_formal =
  | Fixed         of string list
  | Any           of string
  | Fixed_and_any of string list * string  (** The syntax of scheme *)

type error_kind =
  | Normal
  | Read
  | File

module Error_kind = struct
  type t = error_kind

  let show = function Normal -> "normal" | Read -> "read" | File -> "file"

  let pp fmt t = Format.fprintf fmt "%s" @@ show t
end

type data =
  | Symbol        of string
  | Number        of string
  | Cons          of data * data
  | True
  | False
  (* Empty list should be specialized. *)
  | Empty_list
  | Closure       of {
      env : env;
      argument_formal : argument_formal;
      body : data;
    }
  | Primitive_fun of primitive_fun
  | Syntax        of special_form

and special_form = env -> data -> data evaluation_result

and primitive_fun = argument_formal * (data -> data evaluation_result)

and env = binding Environment.t

and macro = data -> data evaluation_result

and binding =
  | Value        of data
  | Special_form of special_form
  | Macro        of special_form

and scheme_error =
  | Error_obj    of {
      error_kind : error_kind;
      message : string;
      irritants : data list;
    }
  | Syntax_error of {
      message : string;
      args : data list;
    }

and 'a evaluation_result = ('a, scheme_error) result

let raise_error ?irritants error =
  Error (Error_obj { message = error; error_kind = Normal; irritants = Option.value irritants ~default:[] })

let raise_syntax_error ?args error = Error (Syntax_error { message = error; args = Option.value args ~default:[] })

module Scheme_error = struct
  type t = scheme_error

  let show = function
    | Error_obj { error_kind; message; _ } -> Printf.sprintf "Error(%s): %s" (Error_kind.show error_kind) message
    | Syntax_error { message; _ }          -> Printf.sprintf "Syntax: %s" message

  let pp fmt t = Format.fprintf fmt "%s" @@ show t
end

let is_cons = function Cons _ -> true | _ -> false

let is_symbol = function Symbol _ -> true | _ -> false

let is_true = function True -> true | _ -> false

let is_false = function False -> true | _ -> false

let is_number = function Number _ -> true | _ -> false

let rec is_proper_list v = match v with Empty_list -> true | Cons (_, cdr) -> is_proper_list cdr | _ -> false

module Access = struct
  let symbol_name = function Symbol sym -> Some sym | _ -> None
end

module Constructor = struct
  let cons a b = Cons (a, b)

  let symbol b = Symbol b

  let number v = Number v
end
