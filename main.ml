open Ast
open Defparse
open Deflex
open Unparse

let main () =
  try
    let stmts = ((defparse deflex) (Lexing.from_channel stdin))
    in unparse stmts
  with LexError err ->
    print_endline err; exit 1

let _ = main ()

