{
open Parser
}

(* Define helper regexes *)
let white = [' ' '\t']+
let number = '0' | ['1'-'9']['0'-'9']*

rule read = parse
  | white { read lexbuf }
  | number { NUMBER }
  | eof { EOF }
