open Ast
open Cfg
open Defparse
open Deflex
open Irfactory
open Lexing
open Scrubber
open Util

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
    close_in infile;
    let stmts = scrub stmts in
    let program = convert_ast stmts in
    try
      process_cfg "t.llvm" Sys.argv.(1) program
    with ProcessingError err -> fatal_error err

let () = main ()

