open Ast
open Defparse
open Deflex
open Lexing
open Unparse
open Verify

let set_fname file lexbuf =
  lexbuf.lex_start_p <- { pos_fname = file;
                          pos_lnum = lexbuf.lex_start_p.pos_lnum;
                          pos_bol = lexbuf.lex_start_p.pos_bol;
                          pos_cnum = lexbuf.lex_start_p.pos_cnum };
  lexbuf.lex_curr_p <- { pos_fname = file;
                         pos_lnum = lexbuf.lex_curr_p.pos_lnum;
                         pos_bol = lexbuf.lex_curr_p.pos_bol;
                         pos_cnum = lexbuf.lex_curr_p.pos_cnum };
  lexbuf

let main () =
  if (Array.length Sys.argv - 1) = 0 then
    (print_endline "Need an input file.";
     exit 1)
  else
    let infile = try open_in Sys.argv.(1)
      with _ -> print_endline "Unable to open input file."; exit 1
    in
    let stmts =
      try let lexbuf = set_fname Sys.argv.(1) (Lexing.from_channel infile)
          in ((defparse deflex) lexbuf)
      with LexError err -> print_endline err; exit 1
    in
    let verified_stmts = verify_ast stmts
    in
    unparse verified_stmts; close_in infile

let () = main ()

