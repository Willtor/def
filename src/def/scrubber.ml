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
  let pre_typemap = make_symtab () in
  let typemap = make_symtab () in
  let enummap = make_symtab () in
  let global_varmap = make_symtab () in
  List.iter (fun (nm, tp, _, _, _, _) -> add_symbol pre_typemap nm tp)
    Types.map_builtin_types;
  let read_type = function
    | TypeDecl (p, nm, ({ bare = DefTypeEnum enums } as tp), _, _) ->
       let () =
         List.iteri
           (fun i str -> add_symbol enummap str (i, tp))
           enums
       in
       add_symbol pre_typemap nm tp
    | TypeDecl (_, nm, tp, _, _) ->
       add_symbol pre_typemap nm tp
    | DeclFcn (p, _, nm, tp, _)
    | DefFcn (p, _, _, nm, tp, _, _) -> add_symbol global_varmap nm (p, tp)
    | _ -> ()
  in
  List.iter read_type stmts;

  let disambiguate_type typename oldtp =
    let bared = function
      | Some t -> Some t.bare
      | None -> None
    in
    let rec v t =
      match t.bare with
      | DefTypeUnresolved name ->
         Report.err_internal __FILE__ __LINE__ "unresolved type."
      | DefTypeOpaque nm ->
         (* Sometimes we think a type is opaque if the parent type was resolved
            before the child. *)
         begin match bared (lookup_symbol pre_typemap nm) with
         | Some (DefTypeLiteralStruct _)
         | Some (DefTypeLiteralUnion _) ->
            maketype t.dtpos (DefTypeNamed nm)
         | Some tp -> maketype t.dtpos tp
         | None -> maketype t.dtpos (DefTypeOpaque nm)
         end
      | DefTypeFcn (params, ret, variadic) ->
         let params = List.map v params
         and ret = v ret
         in maketype None (DefTypeFcn (params, ret, variadic))
      | DefTypePtr tp -> maketype None (DefTypePtr (v tp))
      | DefTypeArray (tp, n) -> maketype None (DefTypeArray (v tp, n))
      | DefTypeLiteralStruct (is_packed, fields, names) ->
         maketype t.dtpos
           (DefTypeLiteralStruct (is_packed, List.map v fields, names))
      | DefTypeStaticStruct (is_packed, members) ->
         maketype t.dtpos
           (DefTypeStaticStruct (is_packed, List.map v members))
      | DefTypeLiteralUnion (fields, names) ->
         maketype t.dtpos (DefTypeLiteralUnion (List.map v fields, names))
      | _ -> t
    in
    add_symbol typemap typename (v oldtp)
  in
  symtab_iter disambiguate_type pre_typemap;

  let rec implicit_cast to_tp expr =
    if equivalent_types to_tp expr.expr_tp then expr
    else if expr.expr_tp.bare = DefTypeWildcard then expr
    else
      match to_tp.bare, expr.expr_ast with
      | DefTypeLiteralStruct (_, mtypes, _), ExprStaticStruct (p, mexprs) ->
         (* Special case: need to cast members. *)
         let cast_members = List.map2 implicit_cast mtypes mexprs in
         let pos = Some (pos_of_cr expr.expr_cr) in
         let revised_tp = maketype pos (DefTypeStaticStruct (p, mtypes)) in
         let subexpr =
           { expr_cr = expr.expr_cr;
             expr_tp = revised_tp;
             expr_ast = ExprStaticStruct (p, cast_members)
           }
         in
         { expr_cr = expr.expr_cr;
           expr_tp = to_tp;
           expr_ast = ExprCast (revised_tp, to_tp, subexpr)
         }
      | _ ->
         { expr_cr = expr.expr_cr;
           expr_tp = to_tp;
           expr_ast = ExprCast (expr.expr_tp, to_tp, expr)
         }
  in

  (* Verify an expression can be cast.  If the types are incompatible, or if
     it's implicit and needs to be explicit, this function will report the
     appropriate error and won't return.  Also, it will generate warnings if
     necessary. *)
  let check_castability is_implicit cast_to subexpr =
    let pos = pos_of_cr subexpr.expr_cr in
    let err () = Report.err_type_mismatch pos
                   (string_of_type subexpr.expr_tp)
                   (string_of_type cast_to)
    in
    let cant_cast () =
      Report.err_cant_cast
        pos
        (string_of_type subexpr.expr_tp)
        (string_of_type cast_to)
    in
    let need_explicit () =
      Report.err_need_explicit_cast
        pos
        (string_of_type subexpr.expr_tp)
        (string_of_type cast_to)
    in
    let rec check_concrete can_lose_volatile from_tp to_tp =
      if is_implicit
         && (not can_lose_volatile)
         && from_tp.dtvolatile
         && (not to_tp.dtvolatile) then
        Report.warn_implicit_loss_of_volatility pos;

      (* equivalent_types will catch most cases.  The remaining cases have to
         be matched specially.  The matching code may seem over-broad in
         identifying things as non-castable, but it's only because we have
         already checked whether the types are equivalent. *)
      if equivalent_types from_tp to_tp then ()
      else
        match from_tp.bare, to_tp.bare with
        | DefTypePrimitive _, DefTypePrimitive _ -> ()
        | DefTypePrimitive _, DefTypeEnum _
        | DefTypeEnum _, DefTypePrimitive _ ->
           if is_implicit then need_explicit ()
           else ()
        | DefTypeFcn (p1, r1, v1), DefTypeFcn (p2, r2, v2) ->
           if is_implicit then
             let left = maketype None
                          (DefTypeFcn (List.map (concrete_of None typemap) p1,
                                       concrete_of None typemap r1,
                                       v1))
             and right = maketype None
                           (DefTypeFcn (List.map (concrete_of None typemap) p2,
                                        concrete_of None typemap r2,
                                        v2))
             in
             if equivalent_types left right then ()
             else
               (* If it's implicit, technically the user can cast it, but this
                  is so uncommon that it's probably an error.  Just report it
                  as an error.  If the user explicitly cast it, then have at
                  it. *)
               err ()
           else ()
        | DefTypeNullPtr, DefTypePtr _ -> ()
        | DefTypePtr _, DefTypePtr { bare = DefTypeVoid } -> ()
        | DefTypePtr { bare = DefTypeVoid }, DefTypePtr _ -> ()
        | DefTypePtr l, DefTypePtr r -> check false l r
        | DefTypePtr _, DefTypeFcn _ | DefTypeFcn _, DefTypePtr _ ->
           if is_implicit then need_explicit ()
           else ()
        | DefTypePtr _, DefTypePrimitive PrimU64
        | DefTypePrimitive PrimU64, DefTypePtr _ ->
           (* FIXME: This is 64-bit specific code.  I should really know
              better than to code like this. *)
           if is_implicit then need_explicit ()
           else ()
        | DefTypeArray (s1, _), DefTypePtr s2
        | DefTypePtr s1, DefTypeArray (s2, _) ->
           (* FIXME: Is this what C/C++ do? *)
           if equivalent_types s1 s2 then ()
           else need_explicit ()
        | DefTypeStaticStruct (p1, s1), DefTypeLiteralStruct (p2, s2, _) ->
           let mcheck a b = check false a b in
           if p1 <> p2 then err ()
           else List.iter2 mcheck s1 s2
        | DefTypeLiteralStruct (p1, s1, _), DefTypeStaticStruct (p2, s2) ->
           let mcheck a b =
             if equivalent_types a b then ()
             else err ()
           in
           if p1 <> p2 then err ()
           else List.iter2 mcheck s1 s2
        | DefTypeWildcard, _ | _, DefTypeWildcard -> ()
        | _, DefTypeOpaque nm ->
           let () = prerr_endline ((string_of_type from_tp) ^ " -> "
                                   ^ (string_of_type to_tp))
           in
           let () = match lookup_symbol typemap nm with
             | None -> prerr_endline "  It was unknown."
             | Some t -> prerr_endline ("  known to be: " ^ (string_of_type t))
           in
           Report.err_internal __FILE__ __LINE__ ("Found opaque type: " ^ nm)
        | _ ->
           if is_implicit then err ()
           else cant_cast ()
    and check can_lose_volatile from_tp to_tp =
      match from_tp.bare, to_tp.bare with
      | DefTypeUnresolved _, _ | _, DefTypeUnresolved _ ->
         Report.err_internal __FILE__ __LINE__ "unresolved type"
      | DefTypeNamed n, DefTypeNamed m when n = m ->
         let () = if is_implicit
                     && (not can_lose_volatile)
                     && from_tp.dtvolatile
                     && (not to_tp.dtvolatile) then
                    Report.warn_implicit_loss_of_volatility pos
         in
         ()
      | DefTypeNamed nm, _ ->
         let act_from = Util.the (lookup_symbol typemap nm) in
         (* Preserve volatility. *)
         let vol_from = if from_tp.dtvolatile then volatile_of act_from
                        else act_from
         in
         check can_lose_volatile vol_from to_tp
      | _, DefTypeNamed nm ->
         let act_to = Util.the (lookup_symbol typemap nm) in
         (* Preserve volatility. *)
         let vol_to = if to_tp.dtvolatile then volatile_of act_to
                      else act_to
         in
         check can_lose_volatile from_tp vol_to
      | _ ->
         check_concrete can_lose_volatile from_tp to_tp
    in
    check true subexpr.expr_tp cast_to
  in

  (* Resolve types of expressions and all subexpressions. *)
  let rec resolve varmap expr =
    match expr.expr_ast with
    | ExprNew (dim, tp, inits) ->
       let resolved_inits =
         List.map (fun (p, e) -> p, resolve varmap e) inits
       in
       { expr_cr = expr.expr_cr;
         expr_tp = expr.expr_tp;
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
            let () = check_castability true param resolved_arg in
            let cast_arg = implicit_cast param resolved_arg in
            resolve_args (cast_arg :: accum) (arest, prest)
       in
       begin
         match lookup_symbol varmap name with
         | Some (_, { bare = DefTypeFcn (params, rettp, _) }) ->
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
            let () = check_castability true bool_type left
            and () = check_castability true bool_type right in
            bool_type,
            implicit_cast bool_type left,
            implicit_cast bool_type right
         | OperAssign ->
            let () = check_castability true left.expr_tp right in
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
    | ExprTernaryCond (cond, left, right) ->
       let resolved_cond = resolve varmap cond in
       let () = check_castability true bool_type resolved_cond in
       let rcond = implicit_cast bool_type resolved_cond
       and rleft = resolve varmap left
       and rright = resolve varmap right in
       let tp = most_general_type (pos_of_cr left.expr_cr) typemap
                  [rleft.expr_tp; rright.expr_tp]
       in
       let cast_left = implicit_cast tp rleft
       and cast_right = implicit_cast tp rright in
       { expr_cr = expr.expr_cr;
         expr_tp = tp;
         expr_ast = ExprTernaryCond (rcond, cast_left, cast_right)
       }
    | ExprVar name ->
       let tp, ast = match lookup_symbol varmap name with
         | None -> (* Might be an enum. *)
            begin
              match lookup_symbol enummap name with
              | None -> Report.err_undefined_var (pos_of_cr expr.expr_cr) name
              | Some (i, tp) -> tp, ExprEnum (name, LitU32 (Int32.of_int i))
            end
         | Some (_, tp) -> tp, ExprVar name
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
       let concrete_idx_tp = concrete_of None typemap resolved_idx.expr_tp in
       let () = if not (is_integer_type concrete_idx_tp) then
                  Report.err_non_integer_index
                    (pos_of_cr resolved_idx.expr_cr)
       in
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
            Report.err_non_struct_member_access (pos_of_cr expr.expr_cr)
       in
       let tp = match field with
         | FieldNumber n ->
            begin
              try List.nth ftypes n
              with _ ->
                    let pos =
                      match expr.expr_cr with
                      | CRExpr (PTE_SelectField
                                  (_, _, PT_FieldInt (_, ftok, _, _))) ->
                         ftok.td_pos
                      | CRApproximate p -> p
                      | _ -> Report.err_internal __FILE__ __LINE__
                               "unexpected code relation for FieldNumber."
                    in
                    Report.err_struct_not_enough_fields pos n
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
       (* Verify no variables are being redefined. *)
       let () =
         List.iter
           (fun vtok ->
             match lookup_symbol_local varmap vtok.td_text with
             | None -> ()
             | Some (p, _) ->
                Report.err_redefined_var vtok.td_pos p vtok.td_text)
           vartoks
       in
       let get_rhs expr =
         match expr.expr_ast with
         | ExprBinary op when op.op_op = OperAssign ->
            expr, op, resolve varmap (Util.the op.op_right)
         | _ -> Report.err_internal __FILE__ __LINE__ "Non-assign initializer"
       in
       let rhs_inits = List.map get_rhs inits in
       let user_specified_tp, resolved_tp =
         match tp.bare, rhs_inits with
         | DefTypeUnresolved _, [] ->
            Report.err_no_init_on_inferred_type decl.td_pos
         | DefTypeUnresolved _, _ ->
            let init_tps = List.map (fun (_, _, i) -> i.expr_tp) rhs_inits in
            let general = most_general_type decl.td_pos typemap init_tps in
            begin
              match general with
              | { bare = DefTypeStaticStruct (is_packed, members) } ->
                 false,
                 { dtpos = general.dtpos;
                   bare = DefTypeLiteralStruct (is_packed, members, []);
                   dtvolatile = general.dtvolatile
                 }
              | _ -> false, general
            end
         | _, _ -> true, tp
       in
       let resolved_inits =
         List.map
           (fun (orig, old_op, rhs) ->
             if user_specified_tp then
               check_castability true resolved_tp rhs;
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
           (fun t -> add_symbol varmap t.td_text (t.td_pos, resolved_tp))
           vartoks
       in
       VarDecl (decl, vartoks, resolved_inits, resolved_tp, vis)
    | stmt -> stmt
  in

  let rec stmt_to_stmt fcn rettp varmap stmt =
    match stmt with
    | StmtExpr (p, e) -> StmtExpr (p, resolve varmap e)
    | Block (p, slist) ->
       Block (p, List.map (stmt_to_stmt fcn rettp varmap) slist)
    | DefFcn (p, exported, vis, nm, tp, params, body) ->
       let fvars = push_symtab_scope varmap in
       let lowered_tp = dearray_fcn tp in
       let sub_rettp = match lowered_tp.bare with
         | DefTypeFcn (ptypes, sub_rettp, _) ->
            let () = List.iter
                       (fun ((pp, n), t) -> add_symbol fvars n (pp, t))
                       (List.combine params ptypes)
            in
            sub_rettp
         | _ ->
            Report.err_internal __FILE__ __LINE__ "non-function-type function."
       in
       DefFcn (p, exported, vis, nm, tp, params,
               List.map (stmt_to_stmt nm sub_rettp fvars) body)
    | VarDecl _ -> declare_var varmap stmt
    | InlineStructVarDecl (td, fields, (p, rhs)) ->
       let resolved_rhs = resolve varmap rhs in
       let resolve_field (p, nm, tp) init_tp =
         match tp.bare with
         | DefTypeUnresolved _ ->
            let () = add_symbol varmap nm (p, init_tp) in
            p, nm, init_tp
         | _ ->
            let () = add_symbol varmap nm (p, tp) in
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
             p, List.map (stmt_to_stmt fcn rettp failvars) fstmts)
           maybe_fail
       in
       let bodyvars = push_symtab_scope varmap in
       let resolved_body = List.map (stmt_to_stmt fcn rettp bodyvars) body in
       TransactionBlock (p, resolved_body, resolved_fail)
    | IfStmt (p, cond, ifbody, elsebody_maybe) ->
       let rcond = resolve varmap cond in
       let () = check_castability true bool_type rcond in
       let resolved_cond = implicit_cast bool_type rcond in
       let resolved_else =
         option_map (fun (p, slist) ->
             let evars = push_symtab_scope varmap in
             p, List.map (stmt_to_stmt fcn rettp evars) slist)
         elsebody_maybe
       in
       let ifvars = push_symtab_scope varmap in
       let resolved_if = List.map (stmt_to_stmt fcn rettp ifvars) ifbody in
       IfStmt (p, resolved_cond, resolved_if, resolved_else)
    | ForLoop (p, is_par, init, (cp, cond), iter, p2, body) ->
       let loopvars = push_symtab_scope varmap in
       let resolved_init = option_map (stmt_to_stmt fcn rettp loopvars) init in
       let rcond = resolve loopvars cond in
       let () = check_castability true bool_type rcond in
       let resolved_cond = implicit_cast bool_type rcond in
       let resolved_iter =
         option_map (fun (p, e) -> p, resolve loopvars e) iter in
       let resolved_body = List.map (stmt_to_stmt fcn rettp loopvars) body in
       ForLoop (p, is_par, resolved_init, (cp, resolved_cond), resolved_iter,
                p2, resolved_body)
    | WhileLoop (p, pre, cond, body) ->
       let rcond = resolve varmap cond in
       let () = check_castability true bool_type rcond in
       let resolved_cond = implicit_cast bool_type rcond in
       let bodyvars = push_symtab_scope varmap in
       let resolved_body = List.map (stmt_to_stmt fcn rettp bodyvars) body in
       WhileLoop (p, pre, resolved_cond, resolved_body)
    | SwitchStmt (p, e, cases) ->
       let resolved_e = resolve varmap e in
       let resolve_case (p, fall, case, body) =
         let resolved_case = resolve varmap case in
         let resolved_body = List.map (stmt_to_stmt fcn rettp varmap) body in
         p, fall, resolved_case, resolved_body
       in
       (* FIXME: Special case type-checking for cases. *)
       SwitchStmt (p, resolved_e, List.map resolve_case cases)
    | Return (p, e) ->
       let () = if rettp.bare = DefTypeVoid then
                  Report.err_return_non_void_from_void_fcn p fcn
       in
       let rret = resolve varmap e in
       let () = check_castability true rettp rret in
       Return (p, implicit_cast rettp rret)
    | _ -> stmt
  in
  List.map (stmt_to_stmt "" nil_type global_varmap)
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
  let process fcn rettp body =
    let rec contains_return = function
      | [] -> false
      | Return (p, e) :: _ ->
         if rettp.bare = DefTypeVoid then
           Report.err_return_non_void_from_void_fcn p fcn
         else true
      | ReturnVoid p :: _ ->
         if rettp.bare = DefTypeVoid then true
         else Report.err_return_void_from_non_void_fcn p fcn
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
      | Return (p, _) :: _ ->
         if rettp.bare = DefTypeVoid then
           Report.err_return_non_void_from_void_fcn p fcn
         else true
      | ReturnVoid p :: _ ->
         if rettp.bare = DefTypeVoid then true
         else Report.err_return_void_from_non_void_fcn p fcn
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
    else if rettp.bare = DefTypeVoid then
      (* Implicit return statement.  This is only allowed when the return
         type is void. *)
      List.append body [ ReturnVoid faux_pos ]
    else raise NoReturn
  in
  let toplevel = function
    | DefFcn (pos, doc, vis, name, tp, params, body) ->
       let rettp =
         match tp.bare with
         | DefTypeFcn (_, rettp, _) -> rettp
         | _ -> Report.err_internal __FILE__ __LINE__ "non fcn function."
       in
       begin
         try DefFcn (pos, doc, vis, name, tp, params,
                     process name rettp body)
         with _ -> (* Fixme: Need the end of function position. *)
           Report.err_no_return pos name
       end
    | stmt -> stmt
  in List.map toplevel

let scrub stmts =
  let (<<=) x f = f x in
  resolve_types stmts
  <<= kill_dead_code
  <<= return_all_paths
