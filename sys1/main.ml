(* open Driver
open Ast *)

let () = 
  print_endline (Ast.show_term (Driver.eval (Driver.parse "succ 0")));;
  print_endline (Ast.show_term (Driver.eval (Driver.parse "if true then 0 else pred 0")));;
  print_endline (Ast.show_term (Driver.eval (Driver.parse "if iszero succ 0 then 0 else succ 0")))
