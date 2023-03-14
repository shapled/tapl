%{
open Ast
%}

%token  ZERO
%token  TRUE
%token  FALSE

%token  SUCC
%token  PRED
%token  ISZERO

%token  IF
%token  THEN
%token  ELSE

%token LPAREN
%token RPAREN

%token  EOF

// nonassociative，有二义性就报错
// %nonassoc ELSE

// 入口范式和对应的规则
%start <Ast.term> prog
%%

prog:
    | t = term; EOF { t }
    ;

term:
    | ZERO { TmZero "" }
    | TRUE { TmTrue "" }
    | FALSE { TmFalse "" }
    | ISZERO; t = term { TmIsZero ("", t) }
    | SUCC; t = term { TmSucc ("", t) }
    | PRED; t = term { TmPred ("", t) }
    | IF; t1 = term; THEN; t2 = term; ELSE; t3 = term { TmIf ("", t1, t2, t3) }
    | LPAREN; t = term; RPAREN { t }
    ;
