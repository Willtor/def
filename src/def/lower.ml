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

open Ast
open Cfg
open Types

let lift_lhs_static_structs program =
  let lift fcn =
    (* FIXME: Naming needs to be fixed so there are no conflicts. *)
    (* FIXME: Special-case.  Need something more general/correct. *)
    let rec repair_expr (pos, expr) =
      match expr with
      | Expr_Binary (pos,
                     OperAssign,
                     is_atomic,
                     DefTypeStaticStruct mtypes,
                     Expr_StaticStruct (_, members),
                     rhs) ->
         let decl = { decl_pos = pos;
                      mappedname = "__defstatic"; (* FIXME: unique name. *)
                      vis = VisLocal;
                      is_tls = false;
                      tp = DefTypeStaticStruct mtypes;
                      params = [];
                     } in
         let vars = ("__defstatic", decl) in
         let exprs = List.mapi (fun n (tp, e) ->
           pos,
           Expr_Binary (pos, OperAssign, is_atomic, tp, e,
                        Expr_SelectField
                          (* volatility doesn't matter since it's the lhs
                             of the original assignment. => false. *)
                          (Expr_Variable "__defstatic", n, false)))
           members
         in
         [ vars ],
         (pos, Expr_Binary (pos, OperAssign, is_atomic,
                            DefTypeStaticStruct mtypes,
                            Expr_Variable "__defstatic", rhs)) :: exprs
      | Expr_Binary (pos, OperAssign,
                     is_atomic,
                     DefTypeLiteralStruct (mtypes, mnames),
                     Expr_StaticStruct (_, members),
                     rhs) ->
         let decl = { decl_pos = pos;
                      mappedname = "__defstatic"; (* FIXME: unique name. *)
                      vis = VisLocal;
                      is_tls = false;
                      tp = DefTypeLiteralStruct (mtypes, mnames);
                      params = [];
                    } in
         let vars = ("__defstatic", decl) in
         let exprs = List.mapi (fun n (tp, e) ->
           pos,
           Expr_Binary (pos, OperAssign, is_atomic, tp, e,
                        Expr_SelectField
                          (* volatility doesn't matter since it's the lhs
                             of the original assignment. => false. *)
                          (Expr_Variable "__defstatic", n, false)))
           members
         in
         [ vars ],
         (pos, Expr_Binary (pos, OperAssign, is_atomic,
                            DefTypeLiteralStruct (mtypes, mnames),
                            Expr_Variable "__defstatic", rhs)) :: exprs
      (* FIXME: Need something for static arrays, I think. *)
      | _ -> [], [ (pos, expr) ]
    in
    let visit vars = function
      | BB_Seq (_, block) ->
         let new_vars, elist =
           List.split (List.map repair_expr block.seq_expr) in
         let () = block.seq_expr <- (List.concat elist) in
         List.concat (vars :: new_vars)
      | BB_Cond _ -> vars
      | BB_Term _ -> vars (* FIXME: Need to fix this up, ever? *)
      | BB_Sync _ -> vars
      | _ -> Report.err_internal __FILE__ __LINE__
         "Unexpected basic block type."
    in
    let vars = Cfg.visit_df visit false fcn.local_vars fcn.entry_bb in
    { defn_begin = fcn.defn_begin;
      defn_end = fcn.defn_end;
      name = fcn.name;
      local_vars = vars;
      entry_bb = fcn.entry_bb;
      fcn_cilk_init = fcn.fcn_cilk_init
    }
  in
  { global_decls = program.global_decls;
    initializers = program.initializers;
    fcnlist = List.map lift program.fcnlist;
    deftypemap = program.deftypemap;
    scope_table = program.scope_table
  }

let lower_cfg program =
  let program = lift_lhs_static_structs program in
  program
