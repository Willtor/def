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

type tapir_target
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

(** Detach (spawn) operation. *)
external build_detach :
  llbasicblock -> llbasicblock -> llvalue -> llbuilder -> llvalue
  = "llvm_build_detach"

(** Reattach (continuation after spawn) operation. *)
external build_reattach :
  llbasicblock -> llvalue -> llbuilder -> llvalue
  = "llvm_build_reattach"

(** Sync (join all spawned work in this context) operation. *)
external build_sync :
  llbasicblock -> llvalue -> llbuilder -> llvalue
  = "llvm_build_sync"

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

(** Tapir pass to install Cilky stuff in place of detach/sync instructions. *)
external add_lower_tapir_to_cilk :
  [ `Module ] Llvm.PassManager.t -> tapir_target -> unit
  = "llvm_add_lower_tapir_to_cilk"

(** Tapir pass to spawn loops with recursive divide-and-conquer. *)
external add_loop_spawning :
  [ `Module ] Llvm.PassManager.t -> tapir_target -> unit
  = "llvm_add_loop_spawning"

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

(** Get a new DISubroutineType for the given return value + parameters. *)
external disubroutine_type :
  llcontext -> lldibuilder -> llvalue list -> llvalue
  = "llvm_disubroutine_type"
