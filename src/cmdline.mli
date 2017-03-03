type compilation_level =
  | COMPILE_ASM
  | COMPILE_OBJ
  | COMPILE_BINARY

val opt_level : int ref

val input_files : string list ref

val output_file : string option ref

val comp_depth : compilation_level ref

val compile_llvm : bool ref

val codegen_debug : bool ref

val parse_cmdline : unit -> unit
