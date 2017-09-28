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

(** Return true iff llfcn contains attach/reattach/sync instructions. *)
let is_parallel llfcn =
  true
(*
  let cilky_block bb =
    match instr_end bb with
    | At_start _ -> false (* Should never see this.  Empty block. *)
    | After (*insn=*)_ -> true (* FIXME: Need to add Opcodes. *)
  in
  (* Are you serious?!  Array.exists didn't exist before OCaml 4.03?! *)
  let exists f array =
    let size = Array.length array in
    let rec iter n =
      if n >= size then false
      else if not (f array.(n)) then false
      else iter (n + 1)
    in
    iter 0
  in
  exists cilky_block (basic_blocks llfcn)
 *)

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
    begin
      add_unify_function_exit_nodes unifier;
      add_lower_tapir_to_cilk parallelizer
    end;
  let iter llfcn = match is_parallel llfcn with
    | true -> ignore(PassManager.run_function llfcn unifier)
    | false -> ()
  in
  iter_functions iter mdl;
  ignore(PassManager.run_module mdl parallelizer)
