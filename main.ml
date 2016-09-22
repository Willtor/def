open Arg
open Ast
open Cfg
open Defparse
open Deflex
open Irfactory
open Lexing
open Scrubber
open Util

type compilation_level =
  | COMPILE_LLVM
  | COMPILE_ASM
  | COMPILE_OBJ
  | COMPILE_BINARY

let set_param param v is_set failure_msg =
  if Util.ref_set param v is_set then ()
  else Report.err_param failure_msg

let input_file = ref ""
let input_file_is_set = ref false
let set_input_file v = (* FIXME: Should support multiple input files. *)
  set_param input_file v input_file_is_set
    "Multiple module compilation not yet supported."

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

let parameter_set =
  [ ("-o", String set_output_file,
     "Set the output file.");
    ("-emit-llvm", Unit (fun () -> set_comp_depth COMPILE_LLVM),
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

let build_outfile_name infile_name =
  let base = Filename.chop_extension (Filename.basename infile_name) in
  output_file := match !comp_depth with
  | COMPILE_LLVM -> base ^ ".ll"
  | COMPILE_ASM -> base ^ ".s"
  | COMPILE_OBJ -> base ^ ".o"
  | COMPILE_BINARY -> "a.out"

let dump_asm file lines =
  let out = open_out file in
  List.iter (fun l -> output_string out (l ^ "\n")) lines;
  close_out out

let get_lines input =
  let rec get accum =
    let line, cont =
      try input_line input, true
      with _ -> "", false
    in
    if cont then get (line :: accum)
    else List.rev accum
  in get []

let main () =
  Arg.parse parameter_set anon_arg usage_msg;
  if !input_file = "" then Report.err_no_input_file ();
  if not !output_file_is_set then build_outfile_name !input_file;
  let infile = try open_in !input_file
    with _ -> Report.err_unable_to_open_file !input_file
  in
  let stmts =
    let lexbuf = set_fname !input_file(Lexing.from_channel infile)
    in ((defparse deflex) lexbuf)
  in
  close_in infile;
  let stmts = scrub stmts in
  let program = convert_ast stmts in
  let llvm_module = process_cfg !input_file program in
  if !comp_depth = COMPILE_LLVM then
    (Llvm.print_module !output_file llvm_module;
     exit 0);
  let llvm_str = Llvm.string_of_llmodule llvm_module in
  let llc_in, llc_out = Unix.open_process "llc" in
  output_string llc_out llvm_str;
  close_out llc_out;
  let lines = get_lines llc_in in
  close_in llc_in;
  if !comp_depth = COMPILE_ASM then
    (dump_asm !output_file lines;
     exit 0);
  let as_in, as_out = Unix.open_process ("as -o " ^ !output_file) in
  List.iter (fun l -> output_string as_out (l ^ "\n")) lines;
  close_out as_out;
  close_in as_in;
  exit 0

let () = main ()

