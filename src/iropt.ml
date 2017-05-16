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
open Llvm_scalar_opts
open Llvm_ipo

let create_pm opt_level =
  let pass_manager = PassManager.create () in
  if opt_level > 0 then
    begin
      add_memory_to_register_promotion pass_manager;
      add_lower_expect_intrinsic pass_manager;
      add_gvn pass_manager;
      add_dead_store_elimination pass_manager;
      add_instruction_combination pass_manager;
      add_cfg_simplification pass_manager;
      add_reassociation pass_manager;
      add_loop_unswitch pass_manager;
      add_function_inlining pass_manager
    end;
  pass_manager
