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
open Error
open Lexing
open Operator
open Parsetree
open Types
open Util

(* FIXME: This analysis should be done on the CFG instead of the AST.
   Replace this module. *)

exception NoReturn

(** Basic types. *)
let void_type = maketype None @@ DefTypeVoid
let bool_type = maketype None @@ DefTypePrimitive PrimBool
let char_type = maketype None @@ DefTypePrimitive PrimI8
let i32_type = maketype None @@ DefTypePrimitive PrimI32
let i64_type = maketype None @@ DefTypePrimitive PrimI64
let string_type =
  makeptr @@ maketype None (DefTypePrimitive PrimI8)
let nil_type = maketype None @@ DefTypeNullPtr
let wildcard_type = maketype None @@ DefTypeWildcard

(* FIXME: This function should be in ast.ml. *)
let position_of_stmt = function
  | StmtExpr (pos, _)
  | Block (pos, _)
  | DeclFcn (pos, _, _, _, _)
  | DefFcn (pos, _, _, _, _, _, _)
  | VarDecl ({td_pos = pos}, _, _, _, _)
  | InlineStructVarDecl ({td_pos = pos}, _, _)
  | TransactionBlock (pos, _, _)
  | IfStmt (pos, _, _, _)
  | ForLoop (pos, _, _, _, _, _, _)
  | WhileLoop (pos, _, _, _)
  | SwitchStmt (pos, _, _)
  | Return (pos, _)
  | ReturnVoid pos
  | TypeDecl (pos, _, _, _, _)
  | Label (pos, _)
  | Goto (pos, _)
  | Break pos
  | Continue pos
  | Sync pos
    -> pos
  | Import (tok, _) -> tok.td_pos

let resolve_types stmts =
  let fcnmap = Hashtbl.create 128 in
  let typemap = make_symtab () in
  let enummap = make_symtab () in
  let global_varmap = make_symtab () in
  List.iter (fun (nm, tp, _, _, _, _) -> add_symbol typemap nm tp)
    Types.map_builtin_types;
  let read_type = function
    | TypeDecl (p, nm, ({ bare = DefTypeEnum enums } as tp), _, _) ->
       let () =
         List.iteri
           (fun i str -> add_symbol enummap str (i, tp))
           enums
       in
       add_symbol typemap nm tp
    | TypeDecl (_, nm, tp, _, _) -> add_symbol typemap nm tp
    | DeclFcn (_, _, nm, tp, _)
    | DefFcn (_, _, _, nm, tp, _, _) -> Hashtbl.add fcnmap nm tp
    | _ -> ()
  in
  List.iter read_type stmts;

  let implicit_cast to_tp expr =
    if equivalent_types to_tp expr.expr_tp then expr
    else if expr.expr_tp.bare = DefTypeWildcard then expr
    else { expr_cr = expr.expr_cr;
           expr_tp = to_tp;
           expr_ast = ExprCast (expr.expr_tp, to_tp, expr)
         }
  in

  (* Resolve types of expressions and all subexpressions. *)
  let rec resolve varmap expr =
    match expr.expr_ast with
    | ExprNew (dim, tp, inits) ->
       let resolved_inits =
         List.map (fun (p, e) -> p, resolve varmap e) inits
       in
       { expr_cr = expr.expr_cr;
         expr_tp = makeptr tp;
         expr_ast = ExprNew (resolve varmap dim, tp, resolved_inits)
       }
    | ExprFcnCall (name, args, is_spawn) ->
       let rec resolve_args accum = function
         | [], _ ->
            (* If it's the wrong number of arguments, we'll catch that later.
               Right now, it's just filling in types. *)
            List.rev accum
         | arg :: arest, [] ->
            let resolved_arg = resolve varmap arg in
            resolve_args (resolved_arg :: accum) (arest, [])
         | arg :: arest, param :: prest ->
            let resolved_arg = resolve varmap arg in
            let cast_arg = implicit_cast param resolved_arg in
            resolve_args (cast_arg :: accum) (arest, prest)
       in
       begin
         try match Hashtbl.find fcnmap name with
             | { bare = DefTypeFcn (params, rettp, _) } ->
                let resolved_args = resolve_args [] (args, params) in
                { expr_cr = expr.expr_cr;
                  expr_tp = rettp;
                  expr_ast = ExprFcnCall (name, resolved_args, is_spawn)
                }
             | _ -> Report.err_internal __FILE__ __LINE__ "Non-fcn function."
         with _ ->
               match lookup_symbol varmap name with
               | Some ({ bare = DefTypeFcn (params, rettp, _) }) ->
                  let resolved_args = resolve_args [] (args, params) in
                  { expr_cr = expr.expr_cr;
                    expr_tp = rettp;
                    expr_ast = ExprFcnCall (name, resolved_args, is_spawn)
                  }
               | Some _ ->
                  (* FIXME: Need to use err_called_non_fcn. *)
                  Report.err_internal __FILE__ __LINE__ "non-function call"
               | None ->
                  (* Maybe a builtin.  FIXME: Need a more general way of
                     handling builtin functions.  This is sloppy. *)
                  let resolved_args = List.map (resolve varmap) args in
                  let builtin_ret rettp =
                    { expr_cr = expr.expr_cr;
                      expr_tp = rettp;
                      expr_ast = ExprFcnCall (name, resolved_args, is_spawn)
                    }
                  in
                  match name with
                  | "__builtin_cas" -> builtin_ret bool_type
                  | "__builtin_swap" ->
                     builtin_ret (List.nth resolved_args 1).expr_tp
                  | "__builtin_setjmp" -> builtin_ret i32_type
                  | "__builtin_longjmp" -> builtin_ret void_type
                  | "typestr" -> builtin_ret string_type
                  | "cast" ->
                     let rettp = match resolved_args with
                       | { expr_ast = ExprType t } :: _ -> t
                       | _ -> Report.err_bad_args_for_builtin
                                (pos_of_cr expr.expr_cr) "cast"
                     in
                     builtin_ret rettp
                  | _ ->
                     Report.err_unknown_fcn_call (pos_of_cr expr.expr_cr) name
       end
    | ExprString _ -> expr
    | ExprBinary op ->
       let left = resolve varmap op.op_left in
       let right = resolve varmap (Util.the op.op_right) in
       let mgt () = most_general_type op.op_pos typemap
                      [left.expr_tp; right.expr_tp]
       in
       let resolved_tp, cast_left, cast_right =
         match op.op_op with
         | OperLT | OperGT | OperLTE | OperGTE
         | OperEquals | OperNEquals ->
            let t = mgt () in
            bool_type, implicit_cast t left, implicit_cast t right
         | OperLogicalAnd | OperLogicalOr ->
            bool_type, left, right
         | OperAssign ->
            left.expr_tp, left, implicit_cast left.expr_tp right
         | _ ->
            let t = mgt () in
            t, implicit_cast t left, implicit_cast t right
       in
       let resolved_op =
         { op_pos = op.op_pos;
           op_op = op.op_op;
           op_left = cast_left;
           op_right = Some (cast_right);
           op_atomic = op.op_atomic
         }
       in
       { expr_cr = expr.expr_cr;
         expr_tp = resolved_tp;
         expr_ast = ExprBinary resolved_op
       }
    | ExprPreUnary op ->
       let resolved_op =
         { op_pos = op.op_pos;
           op_op = op.op_op;
           op_left = resolve varmap op.op_left;
           op_right = None;
           op_atomic = op.op_atomic
         }
       in
       { expr_cr = expr.expr_cr;
         expr_tp = if op.op_op = OperAddrOf then
                     makeptr resolved_op.op_left.expr_tp
                   else
                     resolved_op.op_left.expr_tp;
         expr_ast = ExprPreUnary resolved_op
       }
    | ExprPostUnary op ->
       let resolved_op =
         { op_pos = op.op_pos;
           op_op = op.op_op;
           op_left = resolve varmap op.op_left;
           op_right = None;
           op_atomic = op.op_atomic
         }
       in
       { expr_cr = expr.expr_cr;
         expr_tp = resolved_op.op_left.expr_tp;
         expr_ast = ExprPostUnary resolved_op
       }
    | ExprVar name ->
       let tp, ast = match lookup_symbol varmap name with
         | None -> (* Might be an enum. *)
            begin
              match lookup_symbol enummap name with
              | None ->
                 begin
                   try Hashtbl.find fcnmap name, ExprVar name
                   with _ ->
                         Report.err_undefined_var (pos_of_cr expr.expr_cr) name
                 end
              | Some (i, tp) -> tp, ExprEnum (name, LitU32 (Int32.of_int i))
            end
         | Some tp -> tp, ExprVar name
       in
       { expr_cr = expr.expr_cr;
         expr_tp = tp;
         expr_ast = ast
       }
    | ExprLit _ -> expr
    | ExprEnum _ -> expr
    | ExprCast (from_tp, to_tp, subexpr) ->
       let resolved_subexpr = resolve varmap subexpr in
       { expr_cr = expr.expr_cr;
         expr_tp = to_tp;
         expr_ast =
           ExprCast (resolved_subexpr.expr_tp, to_tp, resolved_subexpr)
       }
    | ExprIndex (base, idx) ->
       let resolved_base = resolve varmap base
       and resolved_idx = resolve varmap idx in
       let tp = match resolved_base.expr_tp.bare with
         | DefTypePtr subtype -> subtype
         | DefTypeArray (subtype, _) -> subtype
         | _ -> Report.err_index_non_ptr (pos_of_cr expr.expr_cr)
       in
       { expr_cr = expr.expr_cr;
         expr_tp = tp;
         expr_ast = ExprIndex (resolved_base, resolved_idx)
       }
    | ExprSelectField (base, field) ->
       let resolved_base = resolve varmap base in
       let rec get_underlying_type t =
         match t.bare with
         | DefTypeNamed nm ->
            get_underlying_type @@ Util.the (lookup_symbol typemap nm)
         | DefTypePtr subtype ->
            get_underlying_type subtype
         | _ -> t
       in
       let underlying_tp = get_underlying_type resolved_base.expr_tp in
       let ftypes, fnames = match underlying_tp.bare with
         | DefTypeLiteralStruct (_, ftypes, fnames)
         | DefTypeLiteralUnion (ftypes, fnames) -> ftypes, fnames
         | _ ->
            let () = prerr_endline (string_of_type underlying_tp) in
            Report.err_non_struct_member_access (pos_of_cr expr.expr_cr)
       in
       let tp = match field with
         | FieldNumber n ->
            begin
              try List.nth ftypes n
              with _ ->
                Report.err_struct_not_enough_fields (pos_of_cr expr.expr_cr) n
            end
         | FieldName nm ->
            let t, _ = try List.find (fun (t, cmp) -> cmp = nm)
                             (List.combine ftypes fnames)
                       with _ -> Report.err_struct_no_such_member
                                   (pos_of_cr expr.expr_cr) nm
            in
            t
       in
       { expr_cr = expr.expr_cr;
         expr_tp = tp;
         expr_ast = ExprSelectField (resolved_base, field)
       }
    | ExprStaticStruct (is_packed, members) ->
       let resolved_m = List.map (resolve varmap) members in
       let mtypes = List.map (fun m -> m.expr_tp) resolved_m in
       let bare = DefTypeStaticStruct (is_packed, mtypes) in
       let tp = maketype (Some (pos_of_cr expr.expr_cr)) bare in
       { expr_cr = expr.expr_cr;
         expr_tp = tp;
         expr_ast = ExprStaticStruct (is_packed, resolved_m)
       }
    | ExprStaticArray elements ->
       let resolved_e = List.map (resolve varmap) elements in
       let etypes = List.map (fun e -> e.expr_tp) resolved_e in
       let tp = most_general_type (pos_of_cr expr.expr_cr) typemap etypes in
       let bare = DefTypeArray (tp, List.length etypes) in
       let array = maketype (Some (pos_of_cr expr.expr_cr)) bare in
       { expr_cr = expr.expr_cr;
         expr_tp = array;
         expr_ast = ExprStaticArray (List.map (implicit_cast tp) resolved_e)
       }
    | ExprType _ -> expr
    | ExprTypeString _ -> expr
    | ExprNil -> expr
    | ExprWildcard -> expr
  in

  (* Declare a variable (including doing type inference, if necessary). *)
  let declare_var varmap = function
    | VarDecl (decl, vartoks, inits, tp, vis) ->
       let get_rhs expr =
         match expr.expr_ast with
         | ExprBinary op when op.op_op = OperAssign ->
            expr, op, resolve varmap (Util.the op.op_right)
         | _ -> Report.err_internal __FILE__ __LINE__ "Non-assign initializer"
       in
       let rhs_inits = List.map get_rhs inits in
       let resolved_tp =
         match tp.bare, rhs_inits with
         | DefTypeUnresolved _, [] ->
            Report.err_no_init_on_inferred_type decl.td_pos
         | DefTypeUnresolved _, _ ->
            let init_tps = List.map (fun (_, _, i) -> i.expr_tp) rhs_inits in
            let general = most_general_type decl.td_pos typemap init_tps in
            begin
              match general with
              | { bare = DefTypeStaticStruct (is_packed, members) } ->
                 { dtpos = general.dtpos;
                   bare = DefTypeLiteralStruct (is_packed, members, []);
                   dtvolatile = general.dtvolatile
                 }
              | _ -> general
            end
         | _, _ -> tp
       in
       let resolved_inits =
         List.map
           (fun (orig, old_op, rhs) ->
             let op = { op_pos = old_op.op_pos;
                        op_op = old_op.op_op;
                        op_left =
                          { expr_cr = old_op.op_left.expr_cr;
                            expr_tp = resolved_tp;
                            expr_ast = old_op.op_left.expr_ast
                          };
                        op_right = Some (implicit_cast resolved_tp rhs);
                        op_atomic = old_op.op_atomic
                      }
             in
             { expr_cr = orig.expr_cr;
               expr_tp = resolved_tp;
               expr_ast = ExprBinary op
             })
         rhs_inits
       in
       let () =
         List.iter
           (fun t -> add_symbol varmap t.td_text resolved_tp)
           vartoks
       in
       VarDecl (decl, vartoks, resolved_inits, resolved_tp, vis)
    | stmt -> stmt
  in

  let rec stmt_to_stmt varmap stmt =
    match stmt with
    | StmtExpr (p, e) -> StmtExpr (p, resolve varmap e)
    | Block (p, slist) -> Block (p, List.map (stmt_to_stmt varmap) slist)
    | DefFcn (p, exported, vis, nm, tp, params, body) ->
       let fvars = push_symtab_scope varmap in
       let () = match tp.bare with
         | DefTypeFcn (ptypes, _, _) ->
            List.iter
              (fun ((_, n), t) -> add_symbol fvars n t)
              (List.combine params ptypes)
         | _ ->
            Report.err_internal __FILE__ __LINE__ "non-function-type function."
       in
       DefFcn (p, exported, vis, nm, tp, params,
               List.map (stmt_to_stmt fvars) body)
    | VarDecl _ -> declare_var varmap stmt
    | InlineStructVarDecl (td, fields, (p, rhs)) ->
       let resolved_rhs = resolve varmap rhs in
       let resolve_field (p, nm, tp) init_tp =
         match tp.bare with
         | DefTypeUnresolved _ ->
            let () = add_symbol varmap nm init_tp in
            p, nm, init_tp
         | _ ->
            let () = add_symbol varmap nm tp in
            p, nm, tp
       in
       let resolved_tp, resolved_fields =
         match resolved_rhs.expr_tp.bare with
         | DefTypeLiteralStruct (is_packed, tp_list, _)
         | DefTypeStaticStruct (is_packed, tp_list) ->
            { dtpos = resolved_rhs.expr_tp.dtpos;
              bare = DefTypeLiteralStruct (is_packed, tp_list, []);
              dtvolatile = false
            },
            List.map2 resolve_field fields tp_list
         | _ -> Report.err_internal __FILE__ __LINE__ "err resolving fields."
       in
       InlineStructVarDecl (td, resolved_fields,
                            (p, implicit_cast resolved_tp resolved_rhs))
    | TransactionBlock (p, body, maybe_fail) ->
       let resolved_fail =
         option_map (fun (p, fstmts) ->
             let failvars = push_symtab_scope varmap in
             p, List.map (stmt_to_stmt failvars) fstmts)
           maybe_fail
       in
       let bodyvars = push_symtab_scope varmap in
       let resolved_body = List.map (stmt_to_stmt bodyvars) body in
       TransactionBlock (p, resolved_body, resolved_fail)
    | IfStmt (p, cond, ifbody, elsebody_maybe) ->
       let resolved_else =
         option_map (fun (p, slist) ->
             let evars = push_symtab_scope varmap in
             p, List.map (stmt_to_stmt evars) slist)
         elsebody_maybe
       in
       let ifvars = push_symtab_scope varmap in
       let resolved_if = List.map (stmt_to_stmt ifvars) ifbody in
       IfStmt (p, resolve varmap cond, resolved_if, resolved_else)
    | ForLoop (p, is_par, init, (cp, cond), iter, p2, body) ->
       let loopvars = push_symtab_scope varmap in
       let resolved_init = option_map (stmt_to_stmt loopvars) init in
       let resolved_cond = resolve loopvars cond in
       let resolved_iter =
         option_map (fun (p, e) -> p, resolve loopvars e) iter in
       let resolved_body = List.map (stmt_to_stmt loopvars) body in
       ForLoop (p, is_par, resolved_init, (cp, resolved_cond), resolved_iter,
                p2, resolved_body)
    | WhileLoop (p, pre, cond, body) ->
       let resolved_cond = resolve varmap cond in
       let bodyvars = push_symtab_scope varmap in
       let resolved_body = List.map (stmt_to_stmt bodyvars) body in
       WhileLoop (p, pre, resolved_cond, resolved_body)
    | SwitchStmt (p, e, cases) ->
       let resolved_e = resolve varmap e in
       let resolve_case (p, fall, case, body) =
         let resolved_case = resolve varmap case in
         let resolved_body = List.map (stmt_to_stmt varmap) body in
         p, fall, resolved_case, resolved_body
       in
       SwitchStmt (p, resolved_e, List.map resolve_case cases)
    | Return (p, e) ->
       Return (p, resolve varmap e)
    | _ -> stmt
  in
  List.map (stmt_to_stmt global_varmap)
  @@ List.map (declare_var global_varmap) stmts

let kill_dead_code =
  let report_dead_code fcn pos =
    let warn = "Dead code in function \"" ^ fcn ^ "\": "
      ^ (format_position pos) ^ "\n" ^ (show_source pos)
    in warning warn
  in
  let rec process name =
    let rec proc accum = function
      | [] -> List.rev accum
      | StmtExpr _ as stmt :: rest ->
         proc (stmt :: accum) rest
      | Block (pos, slist) :: rest ->
         let stmt = Block (pos, proc [] slist) in
         proc (stmt :: accum) rest
      | DeclFcn _ as stmt :: rest -> proc (stmt :: accum) rest
      | DefFcn (pos, doc, vis, name, tp, params, body) :: rest ->
         let stmt = DefFcn (pos, doc, vis, name, tp, params, process name body)
         in proc (stmt :: accum) rest
      | VarDecl _ as stmt :: rest ->
         proc (stmt :: accum) rest
      | InlineStructVarDecl _ as stmt :: rest ->
         proc (stmt :: accum) rest
      | TransactionBlock (pos, body, None) :: rest ->
         let stmt = TransactionBlock (pos, proc [] body, None) in
         proc (stmt :: accum) rest
      | TransactionBlock (pos, body, Some (fpos, fbody)) :: rest ->
         let stmt = TransactionBlock (pos, proc [] body,
                                      Some (fpos, proc [] fbody)) in
         proc (stmt :: accum) rest
      | IfStmt (pos, cond, thenblk, maybe_else) :: rest ->
         let stmt = IfStmt (pos, cond, proc [] thenblk,
                            match maybe_else with
                            | None -> None
                            | Some (epos, elseblk) ->
                               Some (epos, (proc [] elseblk)))
         in proc (stmt :: accum) rest
      | ForLoop (pos, is_parallel, init, cond, iter, dpos, body) :: rest ->
         let stmt = ForLoop (pos, is_parallel, init, cond, iter, dpos,
                             proc [] body)
         in proc (stmt :: accum) rest
      | WhileLoop (pos, precheck, cond, body) :: rest ->
         let stmt = WhileLoop (pos, precheck, cond, proc [] body)
         in proc (stmt :: accum) rest
      | SwitchStmt (pos, expr, cases) :: rest ->
         let f (cpos, fall, ctor, stmts) = cpos, fall, ctor, proc [] stmts in
         let stmt = SwitchStmt (pos, expr, List.map f cases) in
         proc (stmt :: accum) rest
      | TypeDecl _ as stmt :: rest ->
         proc (stmt :: accum) rest
      | Label _ as stmt :: rest ->
         proc (stmt :: accum) rest
      | Sync _ as stmt :: rest ->
         proc (stmt :: accum) rest
      | (Return _ as stmt) :: rest
      | (ReturnVoid _ as stmt) :: rest
      | (Goto _ as stmt) :: rest
      | (Break _ as stmt) :: rest
      | (Continue _ as stmt) :: rest ->
         let () = match rest with
           | [] -> ()
           | extra :: _ -> report_dead_code name (position_of_stmt extra)
         in List.rev (stmt :: accum)
      | Import _ :: _ ->
         Report.err_internal __FILE__ __LINE__ "import in function."
    in proc []
  in
  let toplevel = function
    | DefFcn (pos, doc, vis, name, tp, params, body) ->
       DefFcn (pos, doc, vis, name, tp, params, process name body)
    | stmt -> stmt
  in List.map toplevel

let return_all_paths =
  let process can_return_void body =
    let rec contains_return = function
      | [] -> false
      | Return _ :: _
      | ReturnVoid _ :: _ -> true
      | Block (_, body) :: rest ->
         if contains_return body then true
         else contains_return rest
      | IfStmt (_, _, tstmts, None) :: rest ->
         if contains_return tstmts then true
         else contains_return rest
      | IfStmt (_, _, tstmts, Some (_, estmts)) :: rest ->
         if contains_return tstmts then true
         else if contains_return estmts then true
         else contains_return rest
      | ForLoop (_, _, _, _, _, _, body) :: rest
      | WhileLoop (_, _, _, body) :: rest ->
         if contains_return body then true
         else contains_return rest
      | SwitchStmt (_, _, cases) :: rest ->
         let do_case res (_, _, _, stmts) =
           if true = res then true
           else contains_return stmts
         in
         if List.fold_left do_case false cases then true
         else contains_return rest
      | DefFcn _ :: rest (* Returns from nested functions don't count. *)
      | _ :: rest -> contains_return rest
    in
    let rec returns_p = function
      | [] -> false
      | Return _ :: _
      | ReturnVoid _ :: _ -> true
      | Block (_, block) :: rest ->
         if returns_p block then true
         else returns_p rest
      | StmtExpr _ :: rest
      | DeclFcn _ :: rest
      | DefFcn _ :: rest
      | VarDecl _ :: rest
      | InlineStructVarDecl _ :: rest
      | TransactionBlock _ :: rest
      | IfStmt (_, _, _, None) :: rest
      | TypeDecl _ :: rest
      | Label _ :: rest
      | Goto _ :: rest (* FIXME: Think about Goto case some more... *)
      | Break _ :: rest (* FIXME: Also the break case. *)
      | Continue _ :: rest (* FIXME: Also, the Continue case. *)
      | Sync _ :: rest
        -> returns_p rest
      | IfStmt (_, _, then_branch, Some (_, else_branch)) :: rest ->
         if (returns_p then_branch) && (returns_p else_branch) then true
         else returns_p rest
      | ForLoop _ :: rest ->
         returns_p rest
      | WhileLoop (_, _, cond, body) :: rest ->
         if provably_always_true cond then contains_return body
         else returns_p rest
      | SwitchStmt _ :: rest ->
         returns_p rest
      | Import _ :: _ ->
         Report.err_internal __FILE__ __LINE__ "import in function."
    in
    if returns_p body then body
    else if can_return_void then
      (* Implicit return statement.  This is only allowed when the return
         type is void. *)
      List.append body [ ReturnVoid faux_pos ]
    else raise NoReturn
  in
  let toplevel = function
    | DefFcn (pos, doc, vis, name, tp, params, body) ->
       let can_return_void = match tp.bare with
         | DefTypeFcn (_, { bare = DefTypeVoid }, _) -> true
         | _ -> false
       in
       begin
         try DefFcn (pos, doc, vis, name, tp, params,
                     process can_return_void body)
         with _ -> (* Fixme: Need the end of function position. *)
           Report.err_no_return pos name
       end
    | stmt -> stmt
  in List.map toplevel

let scrub stmts =
  return_all_paths
  @@ kill_dead_code
  @@ resolve_types stmts
