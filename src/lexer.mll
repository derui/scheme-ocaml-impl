(** This lexer for Scheme *)
{
  (* This part is inserted into the head of the generated file. *)
  open Parser
}

let whitespace = [' ' '\t']
let left_paren = '('
let right_paren = ')'
let alphabet = ['a'-'z' 'A'-'Z']
let numeric = ['0'-'9']
let special_char = ['!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '_' '~']
let symbol_first = alphabet | special_char
let symbol_rest = alphabet | special_char | numeric

rule token = parse
  | '\n' {Lexing.new_line lexbuf; token lexbuf}
  | whitespace+ { token lexbuf }
  | left_paren { LEFT_PAREN }
  | right_paren { RIGHT_PAREN }
  | symbol_first {symbol (Lexing.lexeme lexbuf) lexbuf}
  | numeric {number (Lexing.lexeme lexbuf) lexbuf}
  | eof {EOF}

and symbol first_char = parse
                      | symbol_rest+ {
  SYMBOL (first_char ^ (Lexing.lexeme lexbuf) )
}
                      | _ {SYMBOL first_char}
(* this number lexer supports only little subset of Schema's number *)
and number first_char = parse
                      | numeric+ {
                          NUMBER (first_char ^ (Lexing.lexeme lexbuf) )
                        }
                      | _ {NUMBER first_char}
