open Ast

let dummyinfo: info = ""

(* 检查项是否为数字 *)
let rec isnumbericval t = match t with
  | TmZero(_) -> true
  | TmSucc(_, t1) -> isnumbericval t1
  | _ -> false

(* 检查项是否为值 *)
let isval t = match t with
  | TmTrue(_) -> true
  | TmFalse(_) -> true
  | t when isnumbericval t -> true
  | _ -> false

(* 单步求值器 *)
let rec eval1 t = match t with
  | TmIf(_, TmTrue(_), t2, _) -> t2
  | TmIf(_, TmFalse(_), _, t3) -> t3
  | TmIf(fi, t1, t2, t3) ->
    let t1' = eval1 t1 in
    TmIf(fi, t1', t2, t3)
  | TmSucc(fi, t1) ->
    let t1' = eval1 t1 in
    TmSucc(fi, t1')
  | TmPred(_, TmZero(_)) -> TmZero(dummyinfo)
  | TmPred(_, TmSucc(_, nv1)) when (isnumbericval nv1) -> nv1
  | TmPred(fi, t1) -> 
    let t1' = eval1 t1 in
    TmPred(fi, t1')
  | TmIsZero(_, TmZero(_)) -> TmTrue(dummyinfo)
  | TmIsZero(_, TmSucc(_, nv1)) when (isnumbericval nv1) -> TmFalse(dummyinfo)
  | TmIsZero(fi, t1) ->
    let t1' = eval1 t1 in
    TmIsZero(fi, t1')
  | _ -> raise NoRuleApplies

(* 求值器，求值直到异常 *)
let rec eval t =
  try let t' = eval1 t in
      eval t'
  with NoRuleApplies -> t

(* 语法解析器 *)
let parse (s : string) : term =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast
