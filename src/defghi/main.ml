(* Copyright (C) 2017  DEF Authors

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License
   as published by the Free Software Foundation; either version 2
   of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
   02110-1301, USA.
 *)

open Cmdliner
open Defi
open Deflex
open Defparse
open Header
open Error
open Lexing

type output = OUT_DEFI | OUT_C_HEADER

let syntax_error pos = err_pos "Syntax error:" pos

let file_open_error file =
  fatal_error ("Unable to open file \"" ^ file ^ "\".")

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

let parse_def_file file =
  (* FIXME: Need to unify this code with the [nearly] identical code in
     the compiler source. *)
  let infile = try open_in file
               with _ -> file_open_error file
  in
  let parsetree =
    let lexbuf = set_fname file (Lexing.from_channel infile) in
    try (defparse deflex) lexbuf
    with _ -> syntax_error (lexeme_start_p lexbuf)
  in
  close_in infile;
  parsetree

let defghi output_kind files =
  let dot_def = Str.regexp "\\.def$" in
  let generate suffix generator file =
    generator (parse_def_file file) (Str.global_replace dot_def suffix file)
  in
  match output_kind with
  | OUT_DEFI -> List.map (generate ".defi" make_defi) files
  | OUT_C_HEADER -> List.map (generate ".h" make_header) files

(* Input FILE name(s) *)
let files = Arg.(non_empty & pos_all file [] & info [] ~docv:"FILE")

(* -i and -h *)
let output_kind =
  let doc = "Generate DEF interface files (.defi).  This is the default." in
  let defi = OUT_DEFI, Arg.info ["i"] ~doc in

  let doc = "Generate C header files (.h)." in
  let c_header = OUT_C_HEADER, Arg.info ["h"] ~doc in

  Arg.(value & vflag OUT_DEFI [defi; c_header])

let cmd =
  let doc = "generate header or interface files from DEF source files." in
  let man = [
      `S Manpage.s_description;
      `P "DEF Generate Header or Interface takes one or more DEF source files
          (.def) and outputs C-compatible header (.h) or DEF interface (.defi)
          files.  The .defi file is default.";
      `P "$(b,Note:) defghi does $(i,not) verify program semantics.  It does
          the routine grammar checking necessary to generate output files, but
          does not guarantee that your code can be compiled.";
      `S Manpage.s_bugs;
      `P "Bound to be some.  Report bugs to <willtor@mit.edu>."
    ]
  in
  Term.(const defghi $ output_kind $ files),
  Term.info "defghi" ~version:"v-tbd" ~doc ~exits:Term.default_exits ~man

let () = Term.(exit @@ eval cmd)
