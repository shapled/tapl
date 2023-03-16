open Ast

let termShift d t =
  let rec walk c t = match t with
    | TmVar (x, n) -> if x >= c then TmVar (x+d, n+d)
                      else TmVar (x, n+d)
    | TmAbs (x, t1) -> TmAbs (x, walk (c+1) t1)
    | TmApp (t1, t2) -> TmApp (walk c t1, walk c t2)
  in
  walk 0 t

let termSubst j s t =
  let rec walk c t = match t with
    | TmVar (x, n) -> if x = j + c then termShift c s else TmVar (x, n)
    | TmAbs (x, t1) -> TmAbs (x, walk (c+1) t1)
    | TmApp (t1, t2) -> TmApp (walk c t1, walk c t2)
  in
  walk 0 t

let termSubstTop s t =
  termShift (-1) (termSubst 0 (termShift 1 s) t)

let rec isval _ t = match t with
  | TmAbs (_, _) -> true
  | _ -> false

let rec eval1 ctx t = match t with
  | TmApp (TmAbs (_, t12), v2) when isval ctx v2 ->
      termSubstTop v2 t12
  | TmApp (v1, t2) when isval ctx v1 ->
      let t2' = eval1 ctx t2 in
      TmApp (v1, t2')
  | TmApp (t1, t2) -> 
      let t1' = eval1 ctx t1 in
      TmApp (t1', t2)
  | _ -> raise NoRuleApplies

let rec eval ctx t =
  try let t' = eval1 ctx t in
      eval ctx t'
  with NoRuleApplies -> t
