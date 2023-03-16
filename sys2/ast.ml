type binding = string  (* NameBind *)

type context = (string * binding) list

exception NoRuleApplies

type term =
  | TmVar of int * int
  | TmAbs of string * term
  | TmApp of term * term

let pr = print_string

let ctxlength = List.length

let rec pickfreshname ctx x = match ctx with
  | (key, name)::remain -> if key = x then (remain, name) 
                           else let (ctx', name')  = pickfreshname remain x in ((key, name)::ctx', name')
  | [] -> ([], "")

let index2name ctx x = 
  let _, name = List.nth ctx x in name

let rec printtm ctx t = match t with
  | TmAbs(x, t1) ->
    let (ctx', x') = pickfreshname ctx x in
    pr "(lambda "; pr x'; pr ". "; printtm ctx' t1; pr ")"
  | TmApp(t1, t2) ->
    pr "("; printtm ctx t1; pr " "; printtm ctx t2; pr ")"
  | TmVar(x, n) ->
    if ctxlength ctx = n then
      pr (index2name ctx x)
    else
      pr "[bad index]"
