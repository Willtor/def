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

open Arg
open Config
open Version

let set_param param v is_set failure_msg =
  if Util.ref_set param v is_set then ()
  else Report.err_param failure_msg

let opt_level_is_set = ref false
let set_opt_level v =
  set_param opt_level v  opt_level_is_set
    "Multiple optimization levels set."

let input_file_is_set = ref false
let set_input_file v =
  input_files := v :: !input_files;
  input_file_is_set := true

let output_file_is_set = ref false
let set_output_file v =
  set_param output_file (Some v) output_file_is_set
    "Multiple output files specified."

let comp_depth_is_set = ref false
let set_comp_depth v =
  set_param comp_depth v comp_depth_is_set
    "Multiple compilation levels specified."

let compile_llvm_is_set = ref false
let set_compile_llvm v =
  set_param compile_llvm v compile_llvm_is_set
    "Specified -emit-llvm multiple times."

let codegen_debug_is_set = ref false
let set_codegen_debug () =
  set_param codegen_debug true codegen_debug_is_set
    "Specified -cgdebug multiple times."

(** Print the version information and exit. *)
let print_version () =
  print_endline
    ("def version " ^ (string_of_int version_maj) ^ "."
     ^ (string_of_int version_min) ^ "."
     ^ (string_of_int version_patch) ^ version_suffix
     ^ " (build #" ^ version_build ^ " for LLVM-" ^ llvm_compat
     ^ " on " ^ build_date ^ ")");
  print_endline "Copyright (C) 2017 DEF Authors.";
  print_endline "This is free software; see the source for copying conditions.  There is NO";
  print_endline "warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.";
  exit 0

(* Command line arguments. *)
let parameter_set =
  [ ("-o", String set_output_file,
     "Set the output file.");
    ("-emit-llvm", Unit (fun () -> set_compile_llvm true),
     "Compile the module to llvm IR (.ll extension).");
    ("-S", Unit (fun () -> set_comp_depth COMPILE_ASM),
     "Compile the module to a .s assembly file.");
    ("-c", Unit (fun () -> set_comp_depth COMPILE_OBJ),
     "Compile the module to a .o object file.");
    ("-cgdebug", Unit (fun () -> set_codegen_debug ()),
     "Mode for debugging LLVM code generation.");
    ("-O0", Unit (fun () -> set_opt_level 0),
     "Disable compiler optimizations.");
    ("-O1", Unit (fun () -> set_opt_level 1),
     "Level 1 compiler optimizations (default).");
    ("-O2", Unit (fun () -> set_opt_level 2),
     "Level 2 compiler optimizations.");
    ("-O3", Unit (fun () -> set_opt_level 3),
     "Level 3 compiler optimizations. (identical to -O2 at this time)");
    ("-gh", Unit (fun () -> set_comp_depth COMPILE_GENERATE_HEADER),
     "Generate a C/C++-compatible header file.");
    ("-v", Unit (fun () -> print_version ()),
     "Print version info and exit.")
  ]

let anon_arg = set_input_file

let usage_msg = "def: The DEF compiler."

(** Take the arguments from the command line and set the global options. *)
let parse_cmdline () = Arg.parse parameter_set anon_arg usage_msg
