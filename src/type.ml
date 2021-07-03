open Data_type

(** Definitions for data types in schema *)
type data =
  | Symbol        of string
  | Number        of string
  | Cons          of cell
  | True
  | False
  | Scheme_string of Scheme_char.t list
  (* Empty list should be specialized. *)
  | Empty_list
  | Closure       of {
      env : env;
      argument_formal : Argument_formal.t;
      body : data;
    }
  | Primitive_fun of primitive_fun
  | Syntax        of special_form
  | Macro         of macro_fun
  | Undef

and cell = {
  mutable car : data;
  mutable cdr : data;
}

and scheme_fun = data -> data evaluation_result
(** A type of function in scheme. All scheme's functions has this type. *)

(** Special form has specialized evaluator, so type has only kind of special form. *)
and special_form =
  | S_define
  | S_set_force
  | S_if
  | S_lambda
  | S_quote
  | S_unquote
  | S_unquote_splicing
  | S_quasiquote
  | S_cond_expand

and macro_fun = scheme_fun

and continuation = data -> unit

and primitive_fun = Argument_formal.t * (continuation -> data -> data evaluation_result)

and env = data Environment.t

and scheme_error =
  | Error_obj    of {
      error_kind : Error_kind.t;
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

let rec is_proper_list v = match v with Empty_list -> true | Cons { cdr; _ } -> is_proper_list cdr | _ -> false

module Access = struct
  let symbol_name = function Symbol sym -> Some sym | _ -> None
end

let cons a b = Cons { car = a; cdr = b }

let symbol b = Symbol b

let number v = Number v

let scheme_error_show = function Error_obj v -> v.message | Syntax_error v -> v.message

let scheme_error_pp fmt v = Format.fprintf fmt "%s" @@ scheme_error_show v
