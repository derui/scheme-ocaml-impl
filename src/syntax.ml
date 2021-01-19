type number_of_args = int

type scheme_error = string

type 'a evaluation_result = ('a, scheme_error) result

let raise_error error = Error error

(** The syntax of scheme *)
type data =
  | Symbol     of string
  | Number     of string
  | Cons       of data * data
  | True
  | False
  (* Empty list should be specialized. *)
  | Empty_list
  | Closure    of lambda
  | Native_fun of lambda

and lambda = number_of_args option * (env -> data -> data evaluation_result)

and env = binding Environment.t

and binding =
  | Value        of data
  | Special_form of lambda

module Data = struct
  type t = data

  let is_applicable = function Native_fun _ | Closure _ -> true | _ -> false

  let rec to_string = function
    | Symbol s      -> s
    | Number s      -> s
    | Cons (v1, v2) -> Printf.sprintf "(%s . %s)" (to_string v1) (to_string v2)
    | True          -> "#t"
    | False         -> "#f"
    | Empty_list    -> "()"
    | Native_fun _  -> "<#proc ...>"
    | Closure _     -> "<#closure ...>"
end
