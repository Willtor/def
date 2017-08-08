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

(** Synchronized cmpxchg operation. *)
external build_cmpxchg :
  llvalue -> llvalue -> llvalue -> AtomicOrdering.t ->
  AtomicOrdering.t -> string -> llbuilder -> llvalue
  = "llvm_build_cmpxchg_bytecode" "llvm_build_cmpxchg_native"

(** Detach (spawn) operation. *)
external build_detach :
  llbasicblock -> llbasicblock -> llbuilder -> llvalue
  = "llvm_build_detach"

(** Reattach (continuation after spawn) operation. *)
external build_reattach :
  llbasicblock -> llbuilder -> llvalue
  = "llvm_build_reattach"

(** Sync (join all spawned work in this context) operation. *)
external build_sync :
  llbasicblock -> llbuilder -> llvalue
  = "llvm_build_sync"
