module Argument_formal = struct
  type t =
    | Fixed         of string list
    | Any           of string
    | Fixed_and_any of string list * string  (** The syntax of scheme *)
end

module Error_kind = struct
  type t =
    | Normal
    | Read
    | File

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
