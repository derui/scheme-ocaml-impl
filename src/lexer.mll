(** The lexer for Scheme *)
{
  (* This part is inserted into the head of the generated file. *)
  open Parser
}

let whitespace = [' ' '\t']
let left_paren = '('
let right_paren = ')'
let alphabet = ['a'-'z' 'A'-'Z']
let numeric = ['0'-'9']
let hex_scalar = ['0'-'9' 'a'-'z' 'A'-'Z']+
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
let line_comment = ';'

rule token = parse
  | '\n' {Lexing.new_line lexbuf; token lexbuf}
  | whitespace+ { token lexbuf }
  | line_comment { line_comment lexbuf; token lexbuf }
  | left_paren { LEFT_PAREN }
  | right_paren { RIGHT_PAREN }
  | bool_true { BOOL_TRUE }
  | bool_false { BOOL_FALSE }
  | quasiquote {QUASIQUOTE}
  | quote {QUOTE}
  | comma {UNQUOTE}
  | dot {DOT}
  (* | '"' ("\\a" | "\\b" | "\\t" | "\\n" | "\\r" | "\\\"" | "\\|" | "\\x" hex_scalar ';' | ['\x00'-'\xff'])* as lexeme '"' {STRING lexeme} *)
  | '"' {string [] lexbuf}
  | ellipsis {SYMBOL (Lexing.lexeme lexbuf)}
  | symbol_first {SYMBOL (Lexing.lexeme lexbuf)}
  | symbol_first symbol_rest+ {SYMBOL (Lexing.lexeme lexbuf)}
(* this number lexer supports only little subset of Schema's number *)
  | numeric+ {NUMBER (Lexing.lexeme lexbuf)}
  | eof {EOF}
and line_comment = parse
  | ( '\n' | eof ) {Lexing.new_line lexbuf}
  | _ { line_comment lexbuf }
and string buf = parse
  | "\\a" | "\\b" | "\\t" | "\\n" | "\\r" | "\\\"" | "\\|" | ( "\\x" hex_scalar ';' ) {string  (Lexing.lexeme lexbuf :: buf) lexbuf}
  | '"' { STRING (List.rev buf) }
  | _ { string (Lexing.lexeme lexbuf :: buf) lexbuf }
