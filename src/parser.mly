%{
    open Syntax
    %}

%token <string> SYMBOL
%token <string> NUMBER
%token EOF LEFT_PAREN RIGHT_PAREN
%type <term list> program
%start program

%%

/* should implement from http://www.unixuser.org/~euske/doc/r5rs-ja/r5rs-ja.pdf */

program:
  | term+ EOF { $1 }

term:
  | SYMBOL { Symbol $1}
  | NUMBER { Number $1 }
  | scheme_list { $1 }

scheme_list:
  | LEFT_PAREN RIGHT_PAREN { Empty_list }
  | LEFT_PAREN term+ RIGHT_PAREN { List $2 }
