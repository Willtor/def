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
open Cimportext
open Config
open Defparse
open Deflex
open Irfactory
open Lexing
open Link
open Llvm
open Llvm_all_backends
open Llvm_bitreader
open Llvm_bitwriter
open Llvm_irreader
open Llvm_target
open Lower
open Parsetree
open Scrubber
open Util
open Version

let tm =
  let triple = Target.default_triple () in
  Llvm_all_backends.initialize ();
  TargetMachine.create triple (Target.by_triple triple)

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

let findfile file relative_base pos =
  if file.[0] = '/' then
    if Sys.file_exists file then file
    else Report.err_unable_to_locate_imported_file pos file
  else
    let paths = relative_base :: !import_dirs in
    let exists_in base = Sys.file_exists (base ^ "/" ^ file) in
    let path =
      try List.find exists_in paths
      with _ -> Report.err_unable_to_locate_imported_file pos file
    in
    path ^ "/" ^ file

let add_builtin_fcns stmts =
  let pos = { pos_fname = "builtin";
              pos_lnum = 1;
              pos_bol = 1;
              pos_cnum = 1
            }
  in
  let builtins =
    [ DeclFcn (pos, Types.VisExported pos, "__builtin_xbegin",
               FcnType ([], VarType (pos, "i32", [Types.Volatile])));
      DeclFcn (pos, Types.VisExported pos, "__builtin_xend",
               FcnType ([], VarType (pos, "void", [])));
      DeclFcn (pos, Types.VisExported pos, "forkscan_malloc",
               FcnType ([(pos, "size", VarType (pos, "u64", []))],
                        PtrType (pos, VarType (pos, "void", []), [])));
      DeclFcn (pos, Types.VisExported pos, "forkscan_free",
               FcnType ([(pos, "ptr",
                          PtrType (pos, VarType (pos, "void", []), []))],
                         VarType (pos, "void", [])));
      DeclFcn (pos, Types.VisExported pos, "forkscan_retire",
               FcnType ([(pos, "ptr",
                          PtrType (pos, VarType (pos, "void", []), []))],
                        VarType (pos, "void", [])));
      DeclFcn (pos, Types.VisExported pos, "llvm.x86.xbegin",
               FcnType ([], VarType (pos, "i32", [Types.Volatile])));
      DeclFcn (pos, Types.VisExported pos, "llvm.x86.xend",
               FcnType ([], VarType (pos, "void", [])));
      DeclFcn (pos, Types.VisExported pos, "__defrts_hybrid_xbegin",
               FcnType ([], VarType (pos, "void", [])));
      DeclFcn (pos, Types.VisExported pos, "__defrts_hybrid_xend",
               FcnType ([], VarType (pos, "void", [])));
      DeclFcn (pos, Types.VisExported pos, "llvm.syncregion.start",
               FcnType ([], VarType (pos, "llvm.token", [])))
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

let outfile_name file ext =
  match !output_file with
  | Some ofile -> ofile
  | None -> (Filename.chop_extension (Filename.basename file)) ^ ext

let parse_def_file file =
  let infile = try open_in file
               with _ -> Report.err_unable_to_open_file file
  in
  let parsetree =
    let lexbuf = set_fname file (Lexing.from_channel infile) in
    try (defparse deflex) lexbuf
    with _ -> Report.err_syntax (lexeme_start_p lexbuf)
  in
  close_in infile;
  parsetree

let parse_c_file file =
  let fcns = import_c_file file !import_dirs in fcns

let rec recursive_parse_def_file file =
  let proc parsetree = function
    | PTS_Import (_, (tok, str), _) ->
       let subfile = findfile str (Filename.dirname file) tok.td_pos in
       (recursive_parse_def_file subfile) @ parsetree
    | _ -> parsetree
  in
  match extension file with
  | ".def" | ".defi" ->
     let parsetree = parse_def_file file in
     List.fold_left proc (Ast.of_parsetree parsetree) parsetree
  | ".h" ->
     Ast.of_cimport @@ parse_c_file file
  | _ -> Report.err_internal __FILE__ __LINE__
                             "Need appropriate error message."

let llmodule_of_ast infile ast =
  let expanded_ast = Templates.expand ast in
  let stmts = scrub (add_builtin_fcns expanded_ast) in
  let prog = lower_cfg (Cfg.of_ast stmts) in
  let mdl = process_cfg infile prog in
  let () = Iropt.optimize mdl in
  mdl


let dump_machine_asm file mdl =
  let outfile = outfile_name file ".s" in
  TargetMachine.emit_to_file mdl CodeGenFileType.AssemblyFile outfile tm;
  outfile

let dump_machine_obj tmp_obj file mdl =
  let outfile =
    if not tmp_obj then outfile_name file ".o"
    else "/tmp/"
         ^ (Filename.chop_extension (Filename.basename file))
         ^ (random_hex ()) ^ ".o"
  in
  TargetMachine.emit_to_file mdl CodeGenFileType.ObjectFile outfile tm;
  outfile

let read_ll file =
  let ctx = global_context () in
  let mb = MemoryBuffer.of_file file in
  Llvm_irreader.parse_ir ctx mb

let read_bc file =
  let ctx = global_context () in
  let mb = MemoryBuffer.of_file file in
  Llvm_bitreader.parse_bitcode ctx mb

let generate_asm file =
  match extension file with
  | ".def" ->
     let ast = recursive_parse_def_file file in
     let mdl = llmodule_of_ast file ast in
     if !compile_llvm then
       let outfile = outfile_name file ".ll" in
       (Llvm.print_module outfile mdl; outfile)
     else
       (Iropt.parallelize mdl;
        Iropt.optimize mdl;
        dump_machine_asm file mdl)
  | ".ll" ->
     if !compile_llvm then file
     else
       let mdl = read_ll file in
       let () = Iropt.parallelize mdl in
       let () = Iropt.optimize mdl in
       dump_machine_asm file mdl
  | ".s" ->
     if !compile_llvm then Report.err_cant_convert_s_to_ll file
     else file
  | _ -> Report.err_cant_generate_asm_from file

let generate_obj tmp_obj file =
  match extension file with
  | ".def" ->
     let ast = recursive_parse_def_file file in
     let mdl = llmodule_of_ast file ast in
     let () = Iropt.parallelize mdl in
     let () = Iropt.optimize mdl in
     if !compile_llvm then
       let outfile = outfile_name file ".bc" in
       (* FIXME: Don't ignore bitwriter output. *)
       (ignore(Llvm_bitwriter.write_bitcode_file mdl outfile); outfile)
     else dump_machine_obj tmp_obj file mdl
  | ".ll" ->
     let mdl = read_ll file in
     let () = Iropt.parallelize mdl in
     let () = Iropt.optimize mdl in
     dump_machine_obj tmp_obj file mdl
  | ".s" -> Report.err_cant_generate_obj_from file
  | ".bc" ->
     if !compile_llvm then file
     else dump_machine_obj tmp_obj file (read_bc file)
  | ".o" -> file
  | _ -> Report.err_cant_generate_obj_from file

let compile_input filename =
  match !comp_depth with
  | COMPILE_ASM -> generate_asm filename
  | COMPILE_OBJ -> generate_obj (*tmp-obj=*)false filename
  | COMPILE_BINARY -> generate_obj (*tmp-obj=*)true filename

let pipeline () =
  List.iter verify_extension !input_files;
  let objs = List.map compile_input !input_files in
  match !comp_depth with
  | COMPILE_BINARY ->
     let outfile = match !output_file with
       | None -> "a.out"
       | Some file -> file
     in
     do_linking outfile objs
  | _ -> ()
