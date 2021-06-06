%{
    open Type
    open Data_type
 %}

%token <string> SYMBOL
%token <string> NUMBER
%token <string list> STRING
%token EOF LEFT_PAREN RIGHT_PAREN BOOL_TRUE BOOL_FALSE
%token QUASIQUOTE QUOTE UNQUOTE UNQUOTE_SPLICING DOT
%type <data list> program
%type <data> exp
%start program

%%

/* should implement from http://www.unixuser.org/~euske/doc/r5rs-ja/r7rs-ja.pdf */

program:
  | exp* EOF { $1 }

exp:
  | SYMBOL { Symbol $1}
  | NUMBER { Number $1 }
  | BOOL_FALSE { False }
  | BOOL_TRUE { True }
  | scheme_list { $1 }
  | STRING { Scheme_string (List.map Scheme_char.of_string $1) }
  | QUASIQUOTE exp { Cons {car = Symbol "quasiquote"; cdr = Cons {car = $2; cdr = Empty_list}} }
  | QUOTE exp { Cons {car = Symbol "quote"; cdr =Cons {car = $2; cdr = Empty_list}} }
  | UNQUOTE exp { Cons {car = Symbol "unquote"; cdr = Cons {car = $2; cdr = Empty_list}} }
  | UNQUOTE_SPLICING exp { Cons {car = Symbol "unquote-splicing"; cdr = Cons {car = $2; cdr = Empty_list}} }

scheme_list:
  | LEFT_PAREN scheme_list_body { $2 }

scheme_list_body:
  | RIGHT_PAREN { Empty_list }
  | exp scheme_list_body { Cons {car = $1; cdr = $2} }
  | exp DOT exp RIGHT_PAREN { Cons {car = $1; cdr = $3} }
