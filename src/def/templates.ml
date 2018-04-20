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

(***********************)
(*                     *)
(* FIXME: Unused file. *)
(*                     *)
(***********************)

open Ast
open Parsetree

(** Mangle a name with template parameters. *)
let mangle name params =
  (* FIXME: Need better mangling. *)
  let do_mangle accum param = accum ^ "." ^ param.td_text in
  List.fold_left do_mangle name params

let instantiate templates non_templates =
  let tset = Hashtbl.create 16 in
  let add_template = function
    | DefTemplateFcn (_, _, _, _, name, _, _) as template ->
       Hashtbl.add tset name template
    | _ -> Report.err_internal __FILE__ __LINE__ "non-function template"
  in
  List.iter add_template templates;

  let instantiations = Hashtbl.create 16 in

  let rec specialize template fcn =
    match template with
    | DefTemplateFcn (pos, t, _, vis, name, tp, body) ->
       let tparams = List.map (fun (_, typename) -> typename) t.tmp_args in

       (* map template parameters to instantiated values. *)
       let map = Hashtbl.create 4 in
       let () = List.iter (fun (k, v) -> Hashtbl.add map k.td_text v.td_text)
                          (List.combine tparams fcn.fc_template) in

       let rec map_type = function
         | VarType (pos, name, qlist) ->
            let nm = try Hashtbl.find map name
                     with _ -> name
            in
            VarType (pos, nm, qlist)
         | OpaqueType (pos, nm) ->
            OpaqueType (pos, nm)
         | CVarType _ ->
            Report.err_internal __FILE__ __LINE__
                                "FIXME: instantiate a template with a c type?"
         | FcnType (params, ret) ->
            let f (pos, s, vt) = pos, s, map_type vt in
            FcnType (List.map f params, map_type ret)
         | EnumType (pos, variants) ->
            EnumType (pos, variants)
         | StructType members ->
            let f (pos, s, vt) = pos, s, map_type vt in
            StructType (List.map f members)
         | UnionType members ->
            let f (pos, s, vt) = pos, s, map_type vt in
            UnionType (List.map f members)
         | ArrayType (pos, e, vt) ->
            ArrayType (pos, e, map_type vt)
         | PtrType (pos, vt, qlist) ->
            PtrType (pos, map_type vt, qlist)
         | Ellipsis pos -> Ellipsis pos
         | VAList pos -> VAList pos
         | InferredType -> InferredType
       in

       let mname = mangle name fcn.fc_template in

       let rec do_replace_expr = function
         | ExprNew (pos, tp, inits) ->
            let fix_init (pos, field, idx_opt, fipos, fi) =
              pos, field,
              (if idx_opt = None then None
               else let p, e = Util.the idx_opt in
                    Some (p, do_replace_expr e)),
              fipos, do_replace_expr fi
            in
            ExprNew (pos, map_type tp, List.map fix_init inits)
         | ExprFcnCall f ->
            (* FIXME: the template should be on types, not tokens. *)
            let maybe_replace tok =
              try let v = Hashtbl.find map tok.td_text in
                  { td_pos = f.fc_pos;
                    td_text = v;
                    td_noncode = []
                  }
              with _ -> tok
            in
            let new_template = List.map maybe_replace f.fc_template in
            let mname = mangle f.fc_name new_template in
            let () =
              try ignore(Hashtbl.find tset name)
              with _ ->
                let () = Hashtbl.add instantiations mname None in
                let template =
                  try Hashtbl.find tset f.fc_name
                  with _ ->
                    Report.err_no_such_template f.fc_pos f.fc_name
                in
                let instantiation = specialize template f in
                Hashtbl.replace instantiations mname (Some instantiation)
            in
            ExprFcnCall { fc_pos = f.fc_pos;
                          fc_name = f.fc_name;
                          fc_template = new_template;
                          fc_args = List.map do_replace_expr f.fc_args;
                          fc_spawn = f.fc_spawn
                        }
         | ExprBinary op ->
            ExprBinary { op_pos = op.op_pos;
                         op_op = op.op_op;
                         op_left = do_replace_expr op.op_left;
                         op_right =
                           Some (do_replace_expr (Util.the op.op_right));
                         op_atomic = op.op_atomic
                       }
         | ExprPreUnary op ->
            ExprPreUnary { op_pos = op.op_pos;
                           op_op = op.op_op;
                           op_left = do_replace_expr op.op_left;
                           op_right = None;
                           op_atomic = op.op_atomic
                         }
         | ExprPostUnary op ->
            ExprPostUnary { op_pos = op.op_pos;
                            op_op = op.op_op;
                            op_left = do_replace_expr op.op_left;
                            op_right = None;
                            op_atomic = op.op_atomic
                          }
         | ExprCast (pos, tp, e) ->
            ExprCast (pos, map_type tp, do_replace_expr e)
         | ExprIndex (bpos, base, ipos, idx) ->
            ExprIndex (bpos, do_replace_expr base, ipos, do_replace_expr idx)
         | ExprSelectField (dspos, fpos, ds, field) ->
            ExprSelectField (dspos, fpos, do_replace_expr ds, field)
         | ExprStaticStruct (pos, members) ->
            ExprStaticStruct (pos,
                              List.map (fun (p, e) -> p, do_replace_expr e)
                                       members)
         | ExprType (p, tp) -> ExprType (p, map_type tp)
         | expr -> expr
       in

       let rec do_replace = function
         | StmtExpr (pos, e) -> StmtExpr (pos, do_replace_expr e)
         | Block (pos, body) -> Block (pos, List.map do_replace body)
         | VarDecl (decl, vars, exprs, tp, vis) ->
            VarDecl (decl, vars, List.map do_replace_expr exprs,
                     map_type tp, vis)
         | InlineStructVarDecl (decl, members, (ipos, init)) ->
            let member_replace (p, nm, tp) = p, nm, map_type tp in
            InlineStructVarDecl (decl,
                                 List.map member_replace members,
                                 (ipos, do_replace_expr init))
         | IfStmt (pos, cond, tblock, eblock_opt) ->
            let replaced_eblock_opt = match eblock_opt with
              | None -> None
              | Some (epos, estmts) -> Some (epos, List.map do_replace estmts)
            in
            IfStmt (pos,
                    do_replace_expr cond,
                    List.map do_replace tblock,
                    replaced_eblock_opt)
         | ForLoop (pos, is_parallel, init_opt, (cpos, cond), iter_opt,
                    dpos, body)
           ->
            ForLoop (pos, is_parallel,
                     (if init_opt = None then None
                      else Some (do_replace (Util.the init_opt))),
                     (cpos, do_replace_expr cond),
                     (if iter_opt = None then None
                      else let p, iter = Util.the iter_opt in
                           Some (p, do_replace_expr iter)),
                     dpos,
                     List.map do_replace body)
         | WhileLoop (pos, pre, cond, body) ->
            WhileLoop (pos, pre, do_replace_expr cond,
                       List.map do_replace body)
         | stmt -> stmt
       in

       DefFcn (pos, None, vis, mname, map_type tp, List.map do_replace body)
    | _ ->
       Report.err_internal __FILE__ __LINE__
                           "Called specialize on a non-template."
  in

  let check_fcn = function
    | DefFcn (_, _, _, fcn, _, body) ->
       let get_inst = function
         | ExprFcnCall fcn when fcn.fc_template <> [] ->
            let name = mangle fcn.fc_name fcn.fc_template in
            begin
              try ignore(Hashtbl.find tset name)
              with _ ->
                let () = Hashtbl.add instantiations name None in
                let template =
                  try Hashtbl.find tset fcn.fc_name
                  with _ ->
                    Report.err_no_such_template fcn.fc_pos fcn.fc_name
                in
                let instantiation = specialize template fcn in
                Hashtbl.replace instantiations name (Some instantiation)
            end
         | _ -> ()
       in
       List.iter (Ast.visit_expr_in_stmt get_inst) body
    | _ -> ()
  in

  List.iter check_fcn non_templates;
  Hashtbl.fold (fun _ v accum -> (Util.the v) :: accum) instantiations []

(** Statically expand all templates as required and throw away the templates,
    themselves. *)
let expand ast =
  let is_template_fcn = function
    | DefTemplateFcn _ -> true
    | _ -> false
  in
  let templates, non_templates = List.partition is_template_fcn ast in

  let instantiations = instantiate templates non_templates in
  instantiations @ non_templates
