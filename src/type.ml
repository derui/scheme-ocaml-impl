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

module Scheme_char = struct
  type escape_sequence =
    | Alarm
    | Backspace
    | Tab
    | New_line
    | Carriage_return
    | Double_quote
    | Pipe
    | Scalar          of Int64.t * string

  type t =
    | Escape_sequence of escape_sequence
    | Char            of string

  (* this character holds a byte *)

  let alarm = Escape_sequence Alarm

  let backspace = Escape_sequence Backspace

  let tab = Escape_sequence Tab

  let new_line = Escape_sequence New_line

  let carriage_return = Escape_sequence Carriage_return

  let double_quote = Escape_sequence Double_quote

  let pipe = Escape_sequence Pipe

  let scalar v original = Escape_sequence (Scalar (v, original))

  let hex_of_char = function
    | ('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9') as v -> int_of_char v
    | ('A' | 'B' | 'C' | 'D' | 'E' | 'F') as v -> 10 + (int_of_char v - int_of_char 'A')
    | ('a' | 'b' | 'c' | 'd' | 'e' | 'f') as v -> 10 + (int_of_char v - int_of_char 'a')
    | _ -> failwith "invalid hex character"

  let char_of_hex = function
    | (0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9) as v -> char_of_int (v + int_of_char '0')
    | (10 | 11 | 12 | 13 | 14 | 15) as v -> char_of_int (v + int_of_char 'A')
    | _ -> failwith "invalid hex value"

  let of_string = function
    | "\\a"  -> alarm
    | "\\b"  -> backspace
    | "\\t"  -> tab
    | "\\n"  -> new_line
    | "\\r"  -> carriage_return
    | "\\\"" -> double_quote
    | "\\|"  -> pipe
    | _ as v ->
        if String.index_opt v '\\' = Some 0 && String.index_opt v 'x' = Some 1 && String.rindex_opt v ';' = Some 0 then (
          let v = String.sub v 2 (String.length v - 3) in
          let buf = ref Int64.zero in
          String.iter
            (fun c ->
              let hex = hex_of_char c |> Int64.of_int in
              buf := Int64.(add (shift_left !buf 8) hex))
            v;
          scalar !buf v )
        else Char v

  let to_string = function
    | Escape_sequence Alarm                  -> "\\a"
    | Escape_sequence Backspace              -> "\\b"
    | Escape_sequence Tab                    -> "\\t"
    | Escape_sequence New_line               -> "\\n"
    | Escape_sequence Carriage_return        -> "\\r"
    | Escape_sequence Double_quote           -> "\\\""
    | Escape_sequence Pipe                   -> "\\|"
    | Escape_sequence (Scalar (_, original)) -> "\\x" ^ original ^ ";"
    | Char v                                 -> v
end

(** Definitions for data types in schema *)
type data =
  | Symbol        of string
  | Number        of string
  | Cons          of data * data
  | True
  | False
  | Scheme_string of Scheme_char.t list
  (* Empty list should be specialized. *)
  | Empty_list
  | Closure       of {
      env : env;
      argument_formal : argument_formal;
      body : data;
    }
  | Primitive_fun of primitive_fun
  | Syntax        of special_form

and scheme_fun = data -> data evaluation_result
(** A type of function in scheme. All scheme's functions has this type. *)

and special_form = env -> scheme_fun

and primitive_fun = argument_formal * scheme_fun

and env = binding Environment.t

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
