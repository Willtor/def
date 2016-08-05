open Ast
open Defparse
open Deflex
open Unparse

let main () =
  if (Array.length Sys.argv - 1) = 0 then
    (print_endline "Need an input file.";
     exit 1)
  else
    let infile = try open_in Sys.argv.(1)
      with _ -> print_endline "Unable to open input file."; exit 1
    in
    try
      let stmts = ((defparse deflex) (Lexing.from_channel infile))
      in unparse stmts
    with LexError err ->
      print_endline err; exit 1

let _ = main ()

