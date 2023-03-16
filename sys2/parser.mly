%{
open Ast
%}

%token <int> Number
%token EOF

// 入口范式和对应的规则
%start <term> prog
%%

prog:
    | t = term; EOF { t }
    ;

term:
    | n = Number; { TmVar (1, n) }
    ;
