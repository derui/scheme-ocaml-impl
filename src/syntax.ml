(** The syntax of scheme *)
type term =
  | Symbol     of string
  | Number     of string
  | List       of term list
  | Empty_list
