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
let bool_false = '#' 'f'
let bool_true = '#' 't'
let dot = '.'
let special_char = ['!' '$' '%' '&' '*' '+' '-' '/' ':' '<' '=' '>' '?' '@' '^' '_' '~']
let ellipsis = '.' '.' '.'
let symbol_first = alphabet | special_char
let symbol_rest = alphabet | special_char | dot | numeric
let quasiquote = '`'
let quote = '\''
let comma = ','

rule token = parse
  | '\n' {Lexing.new_line lexbuf; token lexbuf}
  | whitespace+ { token lexbuf }
  | left_paren { LEFT_PAREN }
  | right_paren { RIGHT_PAREN }
  | bool_true { BOOL_TRUE }
  | bool_false { BOOL_FALSE }
  | quasiquote {QUASIQUOTE}
  | quote {QUOTE}
  | comma {UNQUOTE}
  | dot {DOT}
  | ellipsis {SYMBOL (Lexing.lexeme lexbuf)}
  | symbol_first {SYMBOL (Lexing.lexeme lexbuf)}
  | symbol_first symbol_rest+ {SYMBOL (Lexing.lexeme lexbuf)}
(* this number lexer supports only little subset of Schema's number *)
  | numeric+ {NUMBER (Lexing.lexeme lexbuf)}
  | eof {EOF}
