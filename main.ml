open Ast
open Defparse
open Deflex

let main () =
  let stmts = ((defparse deflex) (Lexing.from_channel stdin))
  in List.iter (fun s -> print_endline (Ast.stmt2string s)) stmts

let _ = main ()

