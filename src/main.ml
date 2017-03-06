(* Copyright (C) 2017  DEFC Authors

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

open Ast
open Cfg
open Cmdline
open Defparse
open Deflex
open Header
open Irfactory
open Lexing
open Lower
open Scrubber
open Util

let llc_bin = "llc-3.9"
let llvm_as_bin = "llvm-as-3.9"
let as_bin = "/usr/bin/as"

(* Copied from "clang -v".  Need to figure out how it generates all these
   paths. *)
let ld_cmd = "/usr/bin/ld -z relro --hash-style=gnu --build-id --eh-frame-hdr -m elf_x86_64 -dynamic-linker /lib64/ld-linux-x86-64.so.2"
let ld_paths = "/usr/bin/../lib/gcc/x86_64-linux-gnu/5.4.0/../../../x86_64-linux-gnu/crt1.o /usr/bin/../lib/gcc/x86_64-linux-gnu/5.4.0/../../../x86_64-linux-gnu/crti.o /usr/bin/../lib/gcc/x86_64-linux-gnu/5.4.0/crtbegin.o -L/usr/bin/../lib/gcc/x86_64-linux-gnu/5.4.0 -L/usr/bin/../lib/gcc/x86_64-linux-gnu/5.4.0/../../../x86_64-linux-gnu -L/lib/x86_64-linux-gnu -L/lib/../lib64 -L/usr/lib/x86_64-linux-gnu -L/usr/bin/../lib/gcc/x86_64-linux-gnu/5.4.0/../../.. -L/usr/lib/llvm-3.8/bin/../lib -L/lib -L/usr/lib"
let ld_libs = "-lgcc --as-needed -lgcc_s --no-as-needed -lc -lgcc --as-needed -lgcc_s --no-as-needed /usr/bin/../lib/gcc/x86_64-linux-gnu/5.4.0/crtend.o /usr/bin/../lib/gcc/x86_64-linux-gnu/5.4.0/../../../x86_64-linux-gnu/crtn.o"

let add_builtin_fcns stmts =
  let pos = { pos_fname = "builtin";
              pos_lnum = 1;
              pos_bol = 1;
              pos_cnum = 1
            }
  in
  let builtins =
    [ DeclFcn (pos, Types.VisExported pos, "forkscan_malloc",
               FcnType ([(pos, "size", VarType (pos, "u64"))],
                        PtrType (pos, VarType (pos, "void"))));
      DeclFcn (pos, Types.VisExported pos, "forkscan_free",
               FcnType ([(pos, "ptr", PtrType (pos, VarType (pos, "void")))],
                         VarType (pos, "void")));
      DeclFcn (pos, Types.VisExported pos, "forkscan_retire",
               FcnType ([(pos, "ptr", PtrType (pos, VarType (pos, "void")))],
                         VarType (pos, "void")))
    ]
  in
  builtins @ stmts

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

let obj_name_of file =
  let base = Filename.chop_extension (Filename.basename file) in
  base ^ ".o"

let infile2outfile file =
  match !output_file with
  | Some ofile -> ofile
  | None ->
     let base = Filename.chop_extension (Filename.basename file) in
     match !comp_depth with
     | COMPILE_GENERATE_HEADER -> base ^ ".h"
     | COMPILE_ASM -> if !compile_llvm then base ^ ".ll" else base ^ ".s"
     | COMPILE_OBJ -> if !compile_llvm then base ^ ".bc" else base ^ ".o"
     | COMPILE_BINARY -> "a.out"

let get_lines input =
  let rec get accum =
    let line, cont =
      try input_line input, true
      with _ -> "", false
    in
    if cont then get (line :: accum)
    else List.rev accum
  in get []

let read_file file =
  try
    let fp = open_in file in get_lines fp
  with _ -> Report.err_unable_to_open_file file

let extension str =
  try
    let n = String.rindex str '.' in
    String.sub str n ((String.length str) - n)
  with _ -> ""

let verify_extension filename =
  (* Need OCaml 4.04 for Filename.extension. *)
  match extension filename with
  | ".def"
  | ".ll" | ".s"
  | ".bc" | ".o" -> ()
  | ext -> Report.err_unknown_infile_type filename ext

let dump_bc infilename llvm_txt =
  let outfile = infile2outfile infilename in
  let bc_in, bc_out = Unix.open_process (llvm_as_bin ^ " -o " ^ outfile) in
  List.iter (fun l -> output_string bc_out (l ^ "\n")) llvm_txt;
  close_out bc_out;
  close_in bc_in;
  outfile

let dump_asm infilename lines =
  let outfile = infile2outfile infilename in
  let out = open_out outfile in
  List.iter (fun l -> output_string out (l ^ "\n")) lines;
  close_out out;
  outfile

let compile_asm_lines infilename lines =
  let out =
    if !comp_depth = COMPILE_BINARY then obj_name_of infilename
    else infile2outfile infilename
  in
  let fd_in, fd_out = Unix.pipe () in
  match Unix.fork () with
  | 0 -> 
     begin
       Unix.close fd_out;
       Unix.dup2 fd_in Unix.stdin;
       Unix.execv as_bin [|as_bin; "-o"; out; "-c"|]
     end
  | pid ->
     let () = Unix.close fd_in in
     let oc = Unix.out_channel_of_descr fd_out in
     let () = set_binary_mode_out oc false in
     let () = List.iter (fun l -> output_string oc (l ^ "\n")) lines in
     let () = close_out oc in
     let _, status = Unix.waitpid [] pid in
     begin match status with
     | Unix.WEXITED n
     | Unix.WSIGNALED n
     | Unix.WSTOPPED n -> if n = 0 then out else exit n
     end

let compile_llvm_lines infilename llvm_lines =
  if !compile_llvm && !comp_depth = COMPILE_OBJ then
    dump_bc infilename llvm_lines
  else
    (* Generate ASM. *)
    let llc_in, llc_out = Unix.open_process llc_bin in
    List.iter (fun l -> output_string llc_out (l ^ "\n")) llvm_lines;
    close_out llc_out;
    let lines = get_lines llc_in in
    close_in llc_in;
    if !comp_depth = COMPILE_ASM then
      dump_asm infilename lines
    else
      (* Compile the object file. *)
      compile_asm_lines infilename lines

let compile_bc_file infilename =
  Report.err_internal __FILE__ __LINE__
    ("compile_bc_file not implemented: " ^ infilename)

let compile_def_file infilename =
  let infile = try open_in infilename
    with _ -> Report.err_unable_to_open_file infilename
  in
  let stmts =
    let lexbuf = set_fname infilename (Lexing.from_channel infile)
    in ((defparse deflex) lexbuf)
  in
  close_in infile;

  if !comp_depth = COMPILE_GENERATE_HEADER then
    (header_of (infile2outfile infilename) stmts;
     exit 0);

  (* Generate and process the CFG. *)
  let stmts = add_builtin_fcns stmts in
  let stmts = scrub stmts in
  let program = lower_cfg (convert_ast stmts) in
  let llvm_module =
    process_cfg !codegen_debug infilename program !opt_level in

  (* Output the raw LLVM if that's the compilation level. *)
  if !comp_depth = COMPILE_ASM && !compile_llvm then
    (Llvm.print_module (infile2outfile infilename) llvm_module;
     exit 0);

  (* Process the LLVM. *)
  compile_llvm_lines infilename [(Llvm.string_of_llmodule llvm_module)]

let compile_input filename =
  (* Need OCaml 4.04 for Filename.extension. *)
  match extension filename with
  | ".def" -> compile_def_file filename
  | ".ll" ->
     if not (!comp_depth = COMPILE_ASM && !compile_llvm = true) then
       compile_llvm_lines filename (read_file filename)
     else filename
  | ".s" ->
     if !comp_depth = COMPILE_BINARY ||
       (!comp_depth = COMPILE_OBJ && !compile_llvm = false) then
       compile_asm_lines filename (read_file filename)
     else filename
  | ".bc" ->
     if !comp_depth = COMPILE_BINARY ||
       (!comp_depth = COMPILE_OBJ && !compile_llvm = false) then
       compile_bc_file filename
     else filename
  | ".o" -> filename
  | _ -> Report.err_internal __FILE__ __LINE__
     "unknown filename extension.  Should've been caught earlier."

let main () =
  parse_cmdline ();
  if !input_files = [] then Report.err_no_input_file ();
  List.iter verify_extension !input_files;
  let objects = List.map compile_input !input_files in
  match !comp_depth with
  | COMPILE_BINARY ->
     let outfile = infile2outfile (List.hd objects) in
     let ld_in, ld_out = Unix.open_process
       (ld_cmd ^ " -o " ^ outfile ^ " " ^ ld_paths ^ " "
        ^ (String.concat " " objects)
        ^ " " ^ ld_libs)
     in
     begin
       close_out ld_out;
       close_in ld_in;
     end
  | _ -> ()

let () =
  try
    main ()
  with Failure failure ->
    let msg = failure ^ "\n" ^
      (if Printexc.backtrace_status () then (Printexc.get_backtrace ())
       else "Backtrace disabled.  Use OCAMLRUNPARAM=b to see the stack.")
    in
    fatal_error msg
