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

open Llvm
open Llvm_tapir_opts

type lldibuilder

(** Note: This needs to be kept consistent with metadata.cpp. *)
type dwarf_type =
  | DW_INVALID
  | DW_ATE_BOOLEAN
  | DW_ATE_SIGNED_CHAR
  | DW_ATE_UNSIGNED_CHAR
  | DW_ATE_SIGNED
  | DW_ATE_UNSIGNED
  | DW_ATE_FLOAT

(** Synchronized cmpxchg operation. *)
external build_cmpxchg :
  llvalue -> llvalue -> llvalue -> AtomicOrdering.t ->
  AtomicOrdering.t -> string -> llbuilder -> llvalue
  = "llvm_build_cmpxchg_bytecode" "llvm_build_cmpxchg_native"

external token_type :
  llcontext -> lltype
  = "llvm_token_type"

(** Merge all return nodes into a single node.  This needs to be done before
    Cilkifying a function (lowering detaches/syncs). *)
external add_unify_function_exit_nodes :
  [< Llvm.PassManager.any ] Llvm.PassManager.t -> unit
  = "llvm_add_unify_function_exit_nodes"

(** Create a Cilk target for Tapir to use to convert parallel constructs. *)
external tapir_cilk_target :
  unit -> tapir_target
  = "llvm_tapir_cilk_target"

(** Read a function and determine whether it has detach/sync instructions. *)
external is_parallel :
  llvalue -> bool
  = "llvm_is_parallel"

(* Metadata *)

(** Make a DIBuilder for constructing debugging info. *)
external dibuilder :
  llmodule -> lldibuilder
  = "llvm_dibuilder"

(** Return a DIFile metadata object given the file and path. *)
external difile :
  llcontext -> lldibuilder -> string -> string -> Llvm.llvalue
  = "llvm_difile"

(** Get a compile unit object. *)
external dicompile_unit :
  llcontext -> lldibuilder -> llvalue -> string -> bool -> string -> int
  -> llvalue
  = "llvm_dicompile_unit_bc" "llvm_dicompile_unit"

(** Get a new DIBasicType for the given name, length, and DWARF type. *)
external dibasic_type :
  llcontext -> lldibuilder -> string -> int -> dwarf_type -> llvalue
  = "llvm_dibasic_type_bc" "llvm_dibasic_type"

(** Get a new pointer type to the given base type and pointer size. *)
external dipointer_type :
  llcontext -> lldibuilder -> llvalue -> int -> llvalue
  = "llvm_dipointer_type"

(** Get a new DISubroutineType for the given return value + parameters. *)
external disubroutine_type :
  llcontext -> lldibuilder -> llvalue list -> llvalue
  = "llvm_disubroutine_type"

(** Create debugging info for a struct. *)
external distruct_type :
  llcontext -> lldibuilder -> llvalue -> string -> llvalue -> int -> int
  -> int -> llvalue list -> llvalue
  = "llvm_distruct_type_bc" "llvm_distruct_type"

(** Create debugging info for a typedef. *)
external ditypedef_type :
  llcontext -> lldibuilder -> llvalue -> string -> llvalue -> int -> llvalue
  -> llvalue
  = "llvm_ditypedef_type_bc" "llvm_ditypedef_type"

(** Create debugging info for a function. *)
external difunction :
  llcontext -> lldibuilder -> string -> llvalue -> llvalue -> int -> bool
  -> llvalue -> llvalue
  = "llvm_difunction_bc" "llvm_difunction"

(** Set a function's metadata. *)
external set_subprogram :
  llvalue -> llvalue -> unit
  = "llvm_set_subprogram"

(** Create a DILexicalBlock for the given position. *)
external dilexical_block :
  llcontext -> lldibuilder -> Lexing.position -> llvalue -> llvalue -> llvalue
  = "llvm_dilexical_block"

(** Create a DILocation for the given position. *)
external dilocation :
  llcontext -> Lexing.position -> llvalue -> llvalue
  = "llvm_dilocation"
