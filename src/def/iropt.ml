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
open Llvm_tapir_opts
open Llvmext (* add_unify_function_exit_nodes,
                is_parallel *)

let bldr = Llvm_passmgr_builder.create ()

(** Optimize an LLVM module. *)
let optimize mdl =
  let optimizer = PassManager.create () in
  (* FIXME: Verify !Config.opt_level input. *)
  Llvm_passmgr_builder.set_opt_level !Config.opt_level bldr;
  Llvm_passmgr_builder.populate_module_pass_manager optimizer bldr;
  ignore(PassManager.run_module mdl optimizer)

(** Convert LLVM Tapir primitives to function calls, etc. *)
let parallelize mdl =
  let unifier = PassManager.create_function mdl in
  let parallelizer = PassManager.create () in
  if not (!Config.no_cilk) then
    let tt = tapir_cilk_target () in
    begin
      add_unify_function_exit_nodes unifier;
      add_loop_spawning parallelizer tt;
      add_lower_tapir_to_cilk parallelizer tt
    end;
  (* Re: is_parallel: Why do we do it this way?  Why not make a note when
     parsing the DEF file.  This input may not have come from a DEF source
     file, but from a .ll (or .bc) file.  Therefore, we have to look at the
     actual LLVM instructions. *)
  let maybe_unify llfcn = match is_parallel llfcn with
    | true -> ignore(PassManager.run_function llfcn unifier)
    | false -> ()
  in
  iter_functions maybe_unify mdl;
  ignore(PassManager.run_module mdl parallelizer)
