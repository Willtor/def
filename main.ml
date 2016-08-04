open Ast
open Defparse
open Deflex

let main () =
  try
    let stmts = ((defparse deflex) (Lexing.from_channel stdin))
    in List.iter (fun s -> print_endline (Ast.stmt2string s)) stmts
  with LexError err ->
    print_endline err; exit 1

let _ = main ()

