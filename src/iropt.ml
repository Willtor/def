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

let create_fpm opt_level llvm_module =
  let pass_manager = PassManager.create_function llvm_module in
  if opt_level > 0 then
    begin
      add_instruction_combination pass_manager;
      add_reassociation pass_manager;
      add_gvn pass_manager;
      add_cfg_simplification pass_manager;
      ignore (PassManager.initialize pass_manager)
    end;
  pass_manager
