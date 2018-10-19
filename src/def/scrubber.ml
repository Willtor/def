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

type program =
  { prog_typemap : Types.deftype Util.symtab;
    prog_ast     : Ast.stmt list
  }

exception NoReturn

(** Basic types. *)
let void_type = maketype None @@ DefTypeVoid
let bool_type = maketype None @@ DefTypePrimitive PrimBool
let char_type = maketype None @@ DefTypePrimitive PrimI8
let i32_type = maketype None @@ DefTypePrimitive PrimI32
let i64_type = maketype None @@ DefTypePrimitive PrimI64
let u64_type = maketype None @@ DefTypePrimitive PrimU64
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
    | TypeDecl (p, nm, ({ bare = DefTypeEnum enums } as enum), _, _) ->
       let tp = maketype (Some p) (DefTypeNamed nm) in
       let () =
         List.iteri
           (fun i str -> add_symbol enummap str (i, tp))
           enums
       in
       add_symbol pre_typemap nm enum
    | TypeDecl (_, nm, tp, _, _) ->
       add_symbol pre_typemap nm tp
    | DeclFcn (p, _, nm, tp, _)
    | DefFcn (p, _, _, nm, tp, _, _) ->
       let lowered_type = dearray_fcn tp in
       add_symbol global_varmap nm (p, lowered_type)
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
      | DefTypeNamed name, ExprStaticStruct (p, mexprs) ->
         let revised_ast =
           match concrete_of None typemap to_tp with
           | { bare = DefTypeLiteralStruct (_, mtypes, _) } ->
              let cast_members = List.map2 implicit_cast mtypes mexprs in
              ExprStaticStruct (p, cast_members)
           | _ -> Report.err_internal __FILE__ __LINE__
                    ("unexpected named type: " ^ name)
         in
         { expr_cr = expr.expr_cr;
           expr_tp = to_tp;
           expr_ast = revised_ast
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
         match (concrete_of None typemap tp).bare with
         | DefTypeLiteralStruct (_, mtypes, mnames) ->
            let init_resolve (fname, e) =
              let resolved = resolve varmap e in
              let n = whereis mnames fname.td_text in
              let cast_to = List.nth mtypes n in
              let () = check_castability true cast_to resolved in
              fname, implicit_cast cast_to resolved
            in
            List.map init_resolve inits
         | _ ->
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
                expr_ast = ExprBuiltinCall (name, resolved_args)
              }
            in
            let ret =
              match name with
              | "__builtin_cas" -> builtin_ret bool_type
              | "__builtin_swap" ->
                 builtin_ret (List.nth resolved_args 1).expr_tp
              | "__builtin_setjmp" -> builtin_ret i32_type
              | "__builtin_longjmp" -> builtin_ret void_type
              | "typestr" -> builtin_ret string_type
              | "cast" ->
                 let rettp, e = match resolved_args with
                   | [{ expr_ast = ExprType t }; e] -> t, e
                   | _ -> Report.err_bad_args_for_builtin
                            (pos_of_cr expr.expr_cr) "cast"
                 in
                 { expr_cr = expr.expr_cr;
                   expr_tp = rettp;
                   expr_ast = ExprCast (e.expr_tp, rettp, e)
                 }
              | "__builtin_xbegin" -> builtin_ret i32_type
              | "__builtin_xend" -> builtin_ret void_type
              | "__builtin_xabort" ->
                 let arg = List.hd resolved_args in
                 let () = check_castability true char_type arg in
                 let cast_arg = implicit_cast char_type arg in
                 { expr_cr = expr.expr_cr;
                   expr_tp = void_type;
                   expr_ast = ExprBuiltinCall (name, [cast_arg])
                 }
              | _ ->
                 Report.err_unknown_fcn_call (pos_of_cr expr.expr_cr) name
            in
            if is_spawn then
              Report.err_cant_spawn_builtin (pos_of_cr expr.expr_cr) name
            else
              ret
       end
    | ExprBuiltinCall _ ->
       Report.err_internal __FILE__ __LINE__ "Wasn't expecting a builtin."
    | ExprString _ -> expr
    | ExprBinary op ->
       let left = resolve varmap op.op_left in
       let right = resolve varmap (Util.the op.op_right) in
       let mgt () = most_general_type op.op_pos typemap
                      [left.expr_tp; right.expr_tp]
       in
       let resolved_tp, cast_left, cast_right =
         match op.op_op with
         | OperRemainder
         | OperLShift | OperRShift
         | OperBitwiseAnd | OperBitwiseXor | OperBitwiseOr ->
            let t = mgt () in
            if not (is_integer_type t) then
              Report.err_oper_on_non_integer op.op_pos
                (string_of_operator op.op_op)
                (string_of_type left.expr_tp)
                (string_of_type right.expr_tp)
            else
              t, implicit_cast t left, implicit_cast t right
         | OperLT | OperGT | OperLTE | OperGTE
         | OperEquals | OperNEquals ->
            let t =
              match mgt () with
              | { bare = DefTypePtr _ } -> u64_type
              | { bare = DefTypeNamed name } as t ->
                 begin
                   match concrete_of None typemap t with
                   | { bare = DefTypePtr _ } -> u64_type
                   | _ -> t
                 end
              | t -> t
            in
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
       DefFcn (p, exported, vis, nm, lowered_tp, params,
               List.map (stmt_to_stmt nm sub_rettp fvars) body)
    | VarDecl _ ->
       if fcn <> "" then declare_var varmap stmt
       else stmt
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
         match (concrete_of None typemap resolved_rhs.expr_tp).bare with
         | DefTypeLiteralStruct (is_packed, tp_list, _) ->
            { dtpos = resolved_rhs.expr_tp.dtpos;
              bare = resolved_rhs.expr_tp.bare;
              dtvolatile = false
            },
            List.map2 resolve_field fields tp_list
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
    | TransactionBlock (p, body, fail_clauses) ->
       let resolve_fail (tok, pattern, fstmts) =
         let rpattern = resolve varmap pattern in
         if rpattern.expr_tp.bare <> (DefTypeNamed "transaction_failure_t")
            && rpattern.expr_tp.bare <> DefTypeWildcard then
           Report.err_transaction_fail_bad_type tok.td_pos
             (string_of_type rpattern.expr_tp);
         let failvars = push_symtab_scope varmap in
         tok, rpattern, List.map (stmt_to_stmt fcn rettp failvars) fstmts
       in
       let bodyvars = push_symtab_scope varmap in
       let rbody = List.map (stmt_to_stmt fcn rettp bodyvars) body in
       TransactionBlock (p, rbody, List.map resolve_fail fail_clauses)
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
    | ReturnVoid pos ->
       let () = if rettp.bare <> DefTypeVoid then
                  Report.err_return_void_from_non_void_fcn pos fcn
       in
       ReturnVoid pos
    | _ -> stmt
  in
  let resolved_stmts = List.map (stmt_to_stmt "" nil_type global_varmap)
                       @@ List.map (declare_var global_varmap) stmts
  in
  { prog_typemap = typemap;
    prog_ast = resolved_stmts
  }

(** Verify that all goto's actually go somewhere.  Along the way, warn the
    programmer if there are any unreferenced labels. *)
let legit_gotos prog =
  let verify name stmts =
    let labels = make_symtab () in
    let gotos = make_symtab () in
    let rec find_labels_n_gotos = function
      | Block (_, stmts) -> List.iter find_labels_n_gotos stmts
      | TransactionBlock (_, body, fclauses) ->
         let () = List.iter find_labels_n_gotos body in
         List.iter
           (fun (_, _, fbody) ->
             List.iter find_labels_n_gotos fbody)
           fclauses
      | IfStmt (_, _, body, None) -> List.iter find_labels_n_gotos body
      | IfStmt (_, _, body, Some (_, ebody)) ->
         let () = List.iter find_labels_n_gotos body in
         List.iter find_labels_n_gotos ebody
      | ForLoop (_, _, _, _, _, _, body) -> List.iter find_labels_n_gotos body
      | WhileLoop (_, _, _, body) -> List.iter find_labels_n_gotos body
      | SwitchStmt (_, _, cases) ->
         List.iter
           (fun (_, _, _, body) -> List.iter find_labels_n_gotos body)
           cases
      | Label (pos, label) -> add_symbol labels label pos
      | Goto (pos, label) -> add_symbol gotos label pos
      | _ -> ()
    in
    List.iter find_labels_n_gotos stmts;
    symtab_iter
      (fun label pos ->
        if None = lookup_symbol labels label then
          Report.err_goto_no_dest pos name label)
      gotos;
    symtab_iter
      (fun label pos ->
        if None = lookup_symbol gotos label then
          Report.warn_unreferenced_label pos name label)
      labels
  in
  let toplevel = function
    | DefFcn (_, _, _, name, _, _, body) -> verify name body
    | _ -> ()
  in
  List.iter toplevel prog.prog_ast;
  prog

(** Verify the legitimacy of breaks and continues in functions. *)
let legit_breaks prog =
  let verify name body =
    let rec traverse can_break = function
      | Block (_, stmts) -> List.iter (traverse can_break) stmts
      | TransactionBlock (_, body, fclauses) ->
         let () = List.iter (traverse can_break) body in
         List.iter
           (fun (_, _, fstmts) -> List.iter (traverse can_break) fstmts)
           fclauses
      | IfStmt (_, _, body, None) -> List.iter (traverse can_break) body
      | IfStmt (_, _, body, Some (_, ebody)) ->
         let () = List.iter (traverse can_break) body in
         List.iter (traverse can_break) ebody
      | ForLoop (_, _, _, _, _, _, body) ->
         List.iter (traverse true) body
      | WhileLoop (_, _, _, body) ->
         List.iter (traverse true) body
      | SwitchStmt (_, _, cases) ->
         List.iter
           (fun (_, _, _, body) -> List.iter (traverse can_break) body)
           cases
      | Break pos ->
         if not can_break then
           Report.err_bad_break pos name
      | Continue pos ->
         if not can_break then
           Report.err_bad_continue pos name
      | _ -> ()
    in
    List.iter (traverse false) body
  in
  let toplevel = function
    | DefFcn (_, _, _, name, _, _, body) -> verify name body
    | _ -> ()
  in
  List.iter toplevel prog.prog_ast;
  prog

(** Verify that parallel constructs don't appear inside of transactions and
    only at certain points within expressions. *)
let legit_parallelism prog =
  let verify name =
    let rec check_no_spawn expr =
      match expr.expr_ast with
      | ExprNew (dim, _, inits) ->
         let () = check_no_spawn dim in
         List.iter (fun (_, e) -> check_no_spawn e) inits
      | ExprFcnCall (_, _, true) ->
         Report.err_bad_spawn_location (pos_of_cr expr.expr_cr) name
      | ExprFcnCall (_, args, _) ->
         List.iter check_no_spawn args
      | ExprBuiltinCall (_, args) ->
         List.iter check_no_spawn args
      | ExprBinary op ->
         let () = check_no_spawn op.op_left in
         check_no_spawn (Util.the op.op_right)
      | ExprPreUnary op | ExprPostUnary op ->
         check_no_spawn op.op_left
      | ExprTernaryCond (cond, a, b) ->
         List.iter check_no_spawn [cond; a; b]
      | ExprCast (_, _, e) ->
         check_no_spawn e
      | ExprIndex (base, idx) ->
         let () = check_no_spawn base in
         check_no_spawn idx
      | ExprSelectField (base, _) ->
         check_no_spawn base
      | ExprStaticStruct (_, members) ->
         List.iter check_no_spawn members
      | ExprStaticArray el ->
         List.iter check_no_spawn el
      | ExprTypeString e ->
         check_no_spawn e
      | _ -> ()
    in
    let check_expr can_par pos expr =
      match expr.expr_ast with
      | ExprFcnCall (_, args, true) ->
         begin
           if not can_par then
             Report.err_spawn_in_transaction pos;
           List.iter check_no_spawn args
         end
      | ExprBinary { op_op = OperAssign;
                     op_left = left;
                     op_right = Some ({ expr_ast =
                                          ExprFcnCall (_, args, true) })} ->
         begin
           if not can_par then
             Report.err_spawn_in_transaction pos;
           check_no_spawn left;
           List.iter check_no_spawn args
         end
      | _ ->
         check_no_spawn expr
    in
    let rec traverse can_par = function
      | StmtExpr (pos, expr) -> check_expr can_par pos expr
      | Block (_, stmts) -> List.iter (traverse can_par) stmts
      | VarDecl (tok, _, inits, _, _) ->
         if 1 = (List.length inits) then
           check_expr can_par tok.td_pos (List.hd inits)
         else
           List.iter check_no_spawn inits
      | InlineStructVarDecl (tok, _, (_, rhs)) ->
         check_expr can_par tok.td_pos rhs
      | TransactionBlock (_, body, fclauses) ->
         let () = List.iter (traverse false) body in
         List.iter
           (fun (_, _, fstmts) -> List.iter (traverse can_par) fstmts)
           fclauses
      | IfStmt (_, cond, body, None) ->
         let () = check_no_spawn cond in
         List.iter (traverse can_par) body
      | IfStmt (_, cond, body, Some (_, ebody)) ->
         let () = check_no_spawn cond in
         let () = List.iter (traverse can_par) body in
         List.iter (traverse can_par) ebody
      | ForLoop (pos, is_par, init_maybe, (_, cond), iter_maybe, _, body) ->
         begin
           if is_par && not can_par then
             Report.err_parfor_in_transaction pos;
           let () =
             match init_maybe with
             | Some (StmtExpr (_, e)) -> check_no_spawn e
             | Some (VarDecl (_, _, inits, _, _)) ->
                List.iter check_no_spawn inits
             | None -> ()
             | _ -> Report.err_internal __FILE__ __LINE__ "non-StmtExpr"
           in
           check_no_spawn cond;
           let () =
             match iter_maybe with
             | Some (_, e) -> check_no_spawn e
             | None -> ()
           in
           List.iter (traverse can_par) body
         end
      | WhileLoop (_, _, cond, body) ->
         let () = check_no_spawn cond in
         List.iter (traverse can_par) body
      | SwitchStmt (_, pattern, cases) ->
         let () = check_no_spawn pattern in
         List.iter
           (fun (_, _, _, body) -> List.iter (traverse can_par) body)
           cases
      | Return (_, e) ->
         check_no_spawn e
      | Sync pos ->
         if not can_par then
           Report.err_sync_in_transaction pos
      | _ -> ()
    in
    List.iter (traverse true)
  in
  let toplevel = function
    | DefFcn (_, _, _, name, _, _, body) -> verify name body
    | _ -> ()
  in
  List.iter toplevel prog.prog_ast;
  prog

(** Remove unused chunks of code and warn the programmer. *)
let kill_dead_code prog =
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
      | TransactionBlock (pos, body, fclauses) :: rest ->
         let pfclauses =
           List.map
             (fun (tok, pattern, fstmts) -> tok, pattern, proc [] fstmts)
             fclauses
         in
         let stmt = TransactionBlock (pos, proc [] body, pfclauses) in
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
    in
    proc []
  in

  let toplevel = function
    | DefFcn (pos, doc, vis, name, tp, params, body) ->
       DefFcn (pos, doc, vis, name, tp, params, process name body)
    | stmt -> stmt
  in
  { prog_typemap = prog.prog_typemap;
    prog_ast = List.map toplevel prog.prog_ast
  }

let return_all_paths prog =
  let rec contains_break_p = function
    | [] -> false
    | Import _ :: rest -> contains_break_p rest
    | StmtExpr _ :: rest -> contains_break_p rest
    | Block (_, body) :: rest ->
       contains_break_p body || contains_break_p rest
    | DeclFcn _ :: rest -> contains_break_p rest
    | DefFcn _ :: rest -> contains_break_p rest
    | VarDecl _ :: rest -> contains_break_p rest
    | InlineStructVarDecl _ :: rest -> contains_break_p rest
    | TransactionBlock (_, body, fclauses) :: rest ->
       let clause_contains curr (_, _, fstmts) =
         curr || (contains_break_p fstmts) in
       contains_break_p body
       || List.fold_left clause_contains false fclauses
    | IfStmt (_, _, then_block, None) :: rest ->
       contains_break_p then_block || contains_break_p rest
    | IfStmt (_, _, then_block, Some (_, else_block)) :: rest ->
       contains_break_p then_block
       || contains_break_p else_block
       || contains_break_p rest
    | ForLoop _ :: rest -> contains_break_p rest
    | WhileLoop _ :: rest -> contains_break_p rest
    | SwitchStmt (_, _, cases) :: rest ->
       let check_case has_break (_, _, _, stmts) =
         has_break || contains_break_p stmts
       in
       List.fold_left check_case false cases || contains_break_p rest
    | Return _ :: rest
    | ReturnVoid _ :: rest
    | TypeDecl _ :: rest
    | Label _ :: rest
    | Goto _ :: rest -> contains_break_p rest
    | Break _ :: _ -> true
    | Continue _ :: rest
    | Sync _ :: rest -> contains_break_p rest
  in

  let process pos name rettp stmts =
    let rec returns_p stmts =
      if stmts = [] then false
      else
        match List.hd (List.rev stmts) with
        | Import _ -> false
        | StmtExpr _ -> false
        | Block (_, body) -> returns_p body
        | DeclFcn _ -> false
        | DefFcn _ -> false
        | VarDecl _ -> false
        | InlineStructVarDecl _ -> false
        | TransactionBlock (_, body, fclauses) ->
           let clause_returns agg (_, _, fstmts) = agg && returns_p fstmts in
           returns_p body
           && List.fold_left clause_returns true fclauses
        | IfStmt (_, _, _, None) -> false
        | IfStmt (_, _, then_stmts, Some (_, else_stmts)) ->
           returns_p then_stmts && returns_p else_stmts
        | ForLoop (_, _, _, (_, cond), _, _, body)
        | WhileLoop (_, _, cond, body) ->
           if not (provably_always_true cond) then false
           else if contains_break_p body then false
           else true
        | SwitchStmt (_, _, cases) ->
           let returns_all (so_far, last) (_, ft, _, stmts) =
             if not so_far then so_far, last
             else if not last && not ft then false, false
             else so_far, returns_p stmts
           in
           let so_far, last =
             List.fold_left returns_all (true, true) cases
           in
           so_far && last
        | Return _ -> true
        | ReturnVoid _ -> true
        | TypeDecl _ -> false
        | Label _ -> false
        | Goto _ ->
           (* Okay, not really.  We'll say true, here, since what we really
              care about in returns_p is whether the code can't run off the
              end, and not whether it _actually_ returns. *)
           true
        | Break _ | Continue _ ->
           (* These are technically errors, but we'll catch them elsewhere. *)
           true
        | Sync _ -> false
    in

    if returns_p stmts then
      (* User was good and kind and provided a return statement. *)
      stmts
    else if rettp.bare = DefTypeVoid then
      (* User didn't provide a return statement, but the return type is void
         so that's okay. *)
      stmts @ [ ReturnVoid faux_pos ]
    else
      (* The user was naughty and didn't provide a return from this non-void
         function. *)
      Report.err_no_return pos name
  in

  let toplevel = function
    | DefFcn (pos, doc, vis, name, tp, params, body) ->
       let rettp =
         match tp.bare with
         | DefTypeFcn (_, rettp, _) -> rettp
         | _ -> Report.err_internal __FILE__ __LINE__ "non fcn function."
       in
       DefFcn (pos, doc, vis, name, tp, params,
               process pos name rettp body)
    | stmt -> stmt
  in
  { prog_typemap = prog.prog_typemap;
    prog_ast = List.map toplevel prog.prog_ast
  }

let scrub stmts =
  let (<<=) x f = f x in
  resolve_types stmts
  <<= legit_gotos
  <<= legit_breaks
  <<= legit_parallelism
  <<= kill_dead_code
  <<= return_all_paths
