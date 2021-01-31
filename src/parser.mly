%{
    open Type
 %}

%token <string> SYMBOL
%token <string> NUMBER
%token EOF LEFT_PAREN RIGHT_PAREN BOOL_TRUE BOOL_FALSE
%token QUASIQUOTE QUOTE UNQUOTE DOT
%type <data list> program
%type <data> exp
%start program

%%

/* should implement from http://www.unixuser.org/~euske/doc/r5rs-ja/r5rs-ja.pdf */

program:
  | exp* EOF { $1 }

exp:
  | SYMBOL { Symbol $1}
  | NUMBER { Number $1 }
  | BOOL_FALSE { False }
  | BOOL_TRUE { True }
  | scheme_list { $1 }
  | QUASIQUOTE exp { Cons (Symbol "quasiquote", Cons ($2, Empty_list)) }
  | QUOTE exp { Cons (Symbol "quote",Cons ($2, Empty_list)) }
  | UNQUOTE exp { Cons (Symbol "unquote", Cons ($2, Empty_list)) }

scheme_list:
  | LEFT_PAREN scheme_list_body { $2 }

scheme_list_body:
  | RIGHT_PAREN { Empty_list }
  | exp scheme_list_body { Cons ($1, $2) }
  | exp DOT exp RIGHT_PAREN { Cons ($1, $3) }
