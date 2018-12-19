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
open Isminterp
open Version
open Util

type output = OUT_DEFI | OUT_C_HEADER

let support_email = "willtor@mit.edu"

let version_string =
  (string_of_int version_maj)
  ^ "." ^ (string_of_int version_min)
  ^ "." ^ (string_of_int version_patch)
  ^ version_suffix

let print_version () =
  print_endline
    ("defghi version " ^ version_string
     ^ " (build #" ^ version_build
     ^ " on " ^ build_date ^ ")");
  print_endline "Copyright (C) 2017 DEF Authors.";
  print_endline "This is free software; see the source for copying conditions.  There is NO";
  print_endline "warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE."

let version_helper = function
  | `Version -> (print_version (); `Version)
  | a -> a

let syntax_error pos = err_pos "Syntax error:" pos

let file_open_error file =
  fatal_error ("Unable to open file \"" ^ file ^ "\".")

let parse_def_file file =
  let infile = try open_in file
               with _ -> file_open_error file
  in
  let bindings = bindings_create () in
  (* FIXME: Will have to do what we're doing in the DEF compiler, here, since
     meta-language bindings will be relevant.  Recursively parse the file and
     do all the checking/evaluating. *)
  let parsetree = Frontend.from_in_channel file infile bindings in
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

(* -i and -c *)
let output_kind =
  let doc = "Generate DEF interface files (.defi).  This is the default." in
  let defi = OUT_DEFI, Arg.info ["i"] ~doc in

  let doc = "Generate C header files (.h)." in
  let c_header = OUT_C_HEADER, Arg.info ["c"] ~doc in

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
      `P ("Bound to be some.  Report bugs to <" ^ support_email ^ ">.")
    ]
  in
  let version = "v" ^ version_string in
  Term.(const defghi $ output_kind $ files),
  Term.info "defghi" ~version ~doc ~exits:Term.default_exits ~man

let () = Term.(exit @@ version_helper @@ eval cmd)
