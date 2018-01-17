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

type compilation_level =
  | COMPILE_ASM
  | COMPILE_OBJ
  | COMPILE_BINARY

let opt_level = ref 1

let no_cilk = ref false

let input_files : string list ref = ref []

let output_file : string option ref = ref None

let linked_libs : string list ref = ref []

let comp_depth = ref COMPILE_BINARY

let compile_llvm = ref false

let debug_symbols = ref false

let codegen_debug = ref false

let import_dirs =
  ref [ "/usr/lib/def";
        "/usr/local/lib/def"
      ]
