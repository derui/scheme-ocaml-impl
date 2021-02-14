type argument_formal =
  | Fixed         of string list
  | Any           of string
  | Fixed_and_any of string list * string

type scheme_error = string

type 'a evaluation_result = ('a, scheme_error) result

let raise_error error = Error error

(** The syntax of scheme *)
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
  | Primitive_fun of primitive_closure
  | Syntax_fun    of special_form

and special_form = env -> data -> data evaluation_result

and primitive_closure = argument_formal * (data -> data evaluation_result)

and env = binding Environment.t

and binding =
  | Value        of data
  | Special_form of special_form

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
