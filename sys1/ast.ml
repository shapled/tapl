(* mock types *)
type info = string
[@@deriving show]

exception NoRuleApplies
[@@deriving show]

(* info 表示代码的编译信息，如文件、行、列等 *)
type term = 
  | TmZero of info
  | TmTrue of info
  | TmFalse of info
  | TmIsZero of info * term
  | TmSucc of info * term
  | TmPred of info * term
  | TmIf of info * term * term * term 
  [@@deriving show]
