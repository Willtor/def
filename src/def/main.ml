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
open Config
open Error
open Version

let support_email = "willtor@mit.edu"

let version_string =
  (string_of_int version_maj)
  ^ "." ^ (string_of_int version_min)
  ^ "." ^ (string_of_int version_patch)
  ^ version_suffix

(** Print the version information and exit. *)
let print_version () =
  print_endline
    ("def version " ^ version_string
     ^ " (build #" ^ version_build
     ^ " on " ^ build_date ^ ")");
  print_endline "Copyright (C) 2017 DEF Authors.";
  print_endline "This is free software; see the source for copying conditions.  There is NO";
  print_endline "warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE."

let version_helper = function
  | `Version -> (print_version (); `Version)
  | a -> a

let def compile_depth llvm cgdebug opt output libs import files =
  (* FIXME: Use libs. *)
  if opt < 0 || opt > 3 then
    fatal_error "Specify an optimization level [0-3].  Use --help.";
  comp_depth := compile_depth;
  compile_llvm := llvm;
  codegen_debug := cgdebug;
  opt_level := opt;
  if output <> "" then
    output_file := Some output;
  linked_libs := libs;
  import_dirs := import @ !import_dirs;
  input_files := files;
  try
    Build.pipeline ()
  with Failure failure ->
    let msg =
      failure ^ "\n" ^
        (if Printexc.backtrace_status () then (Printexc.get_backtrace ())
         else "Backtrace disabled.  Use OCAMLRUNPARAM=b to see the stack.")
    in
    fatal_error msg

(* Input file names. *)
let files = Arg.(non_empty & pos_all file [] & info [] ~docv:"FILE(S)")

(* Whether to compile an object, assembly, or a linked binary. *)
let compile_depth =
  let doc = "Compile a native object file (.o) for each input file, and don't
             try to link the objects."
  in
  let obj = COMPILE_OBJ, Arg.info ["c"] ~doc in

  let doc = "Compile a native assembly file (.s) for each input file." in
  let asm = COMPILE_ASM, Arg.info ["S"] ~doc in

  Arg.(value & vflag COMPILE_BINARY [obj; asm])

(* Whether to generate LLVM (IR or bitcode) output. *)
let llvm =
  let doc = "In conjunction with -S or -c, generate LLVM output instead of
             native output.  For an object file, this means LLVM bitcode (.bc)
             will be generated, and for assembly it will be LLVM IR (.ll)."
  in
  let emit_llvm = Arg.info ["emit-llvm"] ~doc in
  Arg.(value & flag emit_llvm)

(* Whether to test the LLVM output. *)
let cgdebug =
  let doc = "Generate output (for LLVM IR, in conjunction with -S --emit-llvm)
             without verifying its correctness.  This is for debugging."
  in
  let dbg = Arg.info ["cgdebug"] ~doc in
  Arg.(value & flag dbg)

(* Optimization level. *)
let opt =
  let doc = "Optimization level [0 = none; 3 = most]." in
  let docv = "LEVEL" in
  let optarg = Arg.info ["O"] ~doc ~docv in
  Arg.(value & opt int 1 optarg)

(* Output file name. *)
let output =
  let doc = "Specify an output file name." in
  let docv = "OUTFILE" in
  let out = Arg.info ["o"] ~doc ~docv in
  Arg.(value & opt string "" out)

(* Override Cmdliner's default version flag. *)
let ver =
  let doc = "Show version information." in
  let verarg = Arg.info ["v"; "version"] ~doc in
  Arg.(value & flag verarg)

(* Link libraries. *)
let libraries =
  let doc = "Link the specified library.  E.g., if you have libm.so in your
             library path, use -lm to link it."
  in
  let docv = "LIBRARY" in
  let lib = Arg.info ["l"] ~doc ~docv in
  Arg.(value & opt_all string [] lib)

let import_paths =
  let doc = "Specify a path to directory to be used for searching for files
             to import."
  in
  let docv = "PATH" in
  let import = Arg.info ["I"] ~doc ~docv in
  Arg.(value & opt_all string [] import)

let cmd =
  let doc = "compiler for the DEF programming language." in
  let man =
    [ `S Manpage.s_description;
      `P "Compile DEF source files (.def).  The def compiler is designed to
          copy the look-and-feel of the clang compiler for C and C++.";
      `P "Take special note that def uses the option $(b,--emit-llvm),
          instead of clang's $(b,-emit-llvm) option.";
      `S Manpage.s_bugs;
      `P ("Report bugs to <" ^ support_email ^ ">.")
    ]
  in
  let version = "v" ^ version_string in
  Term.(const def $ compile_depth $ llvm $ cgdebug $ opt $ output
        $ libraries $ import_paths $ files),
  Term.info "def" ~version ~doc ~exits:Term.default_exits ~man

let () = Term.(exit @@ version_helper @@ eval cmd)
