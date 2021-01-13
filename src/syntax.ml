(* internal representation for lambda and OCaml's function *)
type number_of_args = int
(** The syntax of scheme *)

type data =
  | Symbol     of string
  | Number     of string
  | Cons       of data * data
  | True
  | False
  (* Empty list should be specialized. *)
  | Empty_list
  | Native_fun of native_fun

and native_fun = number_of_args * (data list -> data)

module Data = struct
  type t = data

  let is_applicable = function Native_fun _ -> true | _ -> false

  let rec to_string = function
    | Symbol s      -> s
    | Number s      -> s
    | Cons (v1, v2) -> Printf.sprintf "(%s . %s)" (to_string v1) (to_string v2)
    | True          -> "#t"
    | False         -> "#f"
    | Empty_list    -> "()"
    | Native_fun _  -> "<#proc ...>"
end

(* A simple environment definition *)
type env = (string * data) list
