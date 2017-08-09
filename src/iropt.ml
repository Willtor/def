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
open Llvm_passmgr_builder
open Llvmext (* add_lower_tapir_to_cilk *)

let create_pm opt_level = (* FIXME: Verify opt_level input. *)
  let bldr = Llvm_passmgr_builder.create () in
  let pass_manager = PassManager.create () in
  Llvm_passmgr_builder.set_opt_level opt_level bldr;
  Llvm_passmgr_builder.populate_module_pass_manager pass_manager bldr;
  add_lower_tapir_to_cilk pass_manager;
  pass_manager
