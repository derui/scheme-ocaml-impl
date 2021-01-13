%{
    open Syntax
 %}

%token <string> SYMBOL
%token <string> NUMBER
%token EOF LEFT_PAREN RIGHT_PAREN
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
  | scheme_list { $1 }

scheme_list:
  | LEFT_PAREN scheme_list_body { $2 }

scheme_list_body:
  | RIGHT_PAREN { Empty_list }
  | exp scheme_list_body { Cons ($1, $2) }
