open Arg
open Ast
open Cfg
open Defparse
open Deflex
open Irfactory
open Lexing
open Lower
open Scrubber
open Util

type compilation_level =
  | COMPILE_ASM
  | COMPILE_OBJ
  | COMPILE_BINARY

let llc_bin = "llc-3.9"
let llvm_as_bin = "llvm-as-3.9"

let set_param param v is_set failure_msg =
  if Util.ref_set param v is_set then ()
  else Report.err_param failure_msg

let input_files = ref []
let input_file_is_set = ref false
let set_input_file v =
  input_files := v :: !input_files;
  input_file_is_set := true

let output_file = ref ""
let output_file_is_set = ref false
let set_output_file v =
  set_param output_file v output_file_is_set
    "Multiple output files specified."

let comp_depth = ref COMPILE_BINARY
let comp_depth_is_set = ref false
let set_comp_depth v =
  set_param comp_depth v comp_depth_is_set
    "Multiple compilation levels specified."

let compile_llvm = ref false
let compile_llvm_is_set = ref false
let set_compile_llvm v =
  set_param compile_llvm v compile_llvm_is_set
    "Specified -emit-llvm multiple times."

let parameter_set =
  [ ("-o", String set_output_file,
     "Set the output file.");
    ("-emit-llvm", Unit (fun () -> set_compile_llvm true),
     "Compile the module to llvm IR (.ll extension).");
    ("-S", Unit (fun () -> set_comp_depth COMPILE_ASM),
     "Compile the module to a .s assembly file.");
    ("-c", Unit (fun () -> set_comp_depth COMPILE_OBJ),
     "Compile the module to a .o object file.") ]

let anon_arg = set_input_file

let usage_msg = "defc: The DEF compiler."

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

let infile2outfile file =
  let base = Filename.chop_extension (Filename.basename file) in
  match !comp_depth with
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
  let out = infile2outfile infilename in
  let as_in, as_out = Unix.open_process ("as -o " ^ out ^ " -c") in
  List.iter (fun l -> output_string as_out (l ^ "\n")) lines;
  close_out as_out;
  close_in as_in;
  out

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

  (* Generate and process the CFG. *)
  let stmts = scrub stmts in
  let program = lower_cfg (convert_ast stmts) in
  let llvm_module = process_cfg infilename program in

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
  Arg.parse parameter_set anon_arg usage_msg;
  if not !input_file_is_set then Report.err_no_input_file ();
  List.iter verify_extension !input_files;
  let objects = List.map compile_input !input_files in
  match !comp_depth with
  | COMPILE_BINARY ->
     Report.err_internal __FILE__ __LINE__ (List.hd objects)
  | _ -> ()

let () = main ()

