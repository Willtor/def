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

open Config
open Llvm
open Llvm_passmgr_builder
open Llvmext (* add_unify_function_exit_nodes,
                add_lower_tapir_to_cilk *)

(** Create optimization pass managers. *)
let create_pm () = (* FIXME: Verify opt_level input. *)
  let bldr = Llvm_passmgr_builder.create () in
  let opt_pass = PassManager.create () in
  let cilk_pass = PassManager.create () in

  Llvm_passmgr_builder.set_opt_level !Config.opt_level bldr;
  Llvm_passmgr_builder.populate_module_pass_manager opt_pass bldr;

  if not (!Config.no_cilk) then
    begin
      add_unify_function_exit_nodes cilk_pass;
      add_lower_tapir_to_cilk cilk_pass
    end;

  opt_pass, cilk_pass
