(* internal representation for lambda and OCaml's function *)
type scheme_error = string

type 'a evaluation_result = ('a, scheme_error) result

type 'a evaluation = Syntax.data -> 'a evaluation_result

type special_form_evaluator = Syntax.data evaluation

let raise_error error = Error error
