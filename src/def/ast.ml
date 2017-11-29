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

open Types
open Lexing
open Parsetree

type literal =
  | LitBool of bool
  | LitI8  of char
  | LitU8  of char
  | LitI16 of int32
  | LitU16 of int32
  | LitI32 of int32
  | LitU32 of int32
  | LitI64 of int64
  | LitU64 of int64
  | LitF32 of float
  | LitF64 of float

type operator =
  | OperIncr | OperDecr | OperAddrOf
  | OperMinus | OperPlus
  | OperLogicalNot | OperBitwiseNot
  | OperMult | OperDiv | OperRemainder
  | OperLShift | OperRShift
  | OperLT | OperGT | OperLTE
  | OperGTE | OperEquals | OperNEquals
  | OperBitwiseAnd | OperBitwiseXor | OperBitwiseOr
  | OperLogicalAnd | OperLogicalOr
  | OperEllipsis
  | OperAssign | OperPlusAssign
  | OperMinusAssign | OperMultAssign
  | OperDivAssign | OperRemAssign
  | OperLShiftAssign | OperRShiftAssign
  | OperBAndAssign | OperBXorAssign | OperBOrAssign

type fcn_call =
  { fc_pos      : position;
    fc_name     : string;
    fc_template : Parsetree.tokendata list;
    fc_args     : expr list;
    fc_spawn    : bool
  }

and operation =
  { op_pos : position;
    op_op : operator;
    op_left : expr;
    op_right : expr option;
    op_atomic : bool
  }

and field_id =
  | FieldNumber of int
  | FieldName of string

and expr =
  | ExprNew of
      position * vartype
      * (position * string
         * (position * expr) option
         * position * expr) list
  | ExprFcnCall of fcn_call
  | ExprString of position * string
  | ExprBinary of operation
  | ExprPreUnary of operation
  | ExprPostUnary of operation
  | ExprVar of position * string
  | ExprLit of position * literal
  | ExprCast of position * vartype * expr
  | ExprIndex of position * expr * position * expr
  | ExprSelectField of position * position * expr * field_id
  | ExprStaticStruct of position * (position * expr) list
  | ExprStaticArray of position * (position * expr) list
  | ExprType of position * vartype
  | ExprTypeString of position * expr
  | ExprNil of position

and vartype =
  | VarType of position * string * Types.qualifier list
  | FcnType of (position * string * vartype) list * vartype
  | StructType of (position * string * vartype) list
  | ArrayType of position * expr * vartype
  | PtrType of position * vartype * Types.qualifier list
  | Ellipsis of position
  | InferredType

type stmt =
  | Import of tokendata * tokendata
  | StmtExpr of position * expr
  | Block of position * stmt list
  | DeclFcn of position * Types.visibility * string * vartype
  | DefFcn of position * string option * Types.visibility * string * vartype
    * stmt list
  | DefTemplateFcn of position * pt_template * string option * Types.visibility
    * string * vartype * stmt list
  | VarDecl of Parsetree.tokendata list * expr list * vartype
  | InlineStructVarDecl of position * (position * string * vartype) list
    * (position * expr)
  | XBegin of position
  | XCommit of position
  | IfStmt of position * expr * stmt list * stmt list option
  (* ForLoop: start-pos * is_parallel * init * cond * iter * body *)
  | ForLoop of position * bool * stmt option * (position * expr)
    * (position * expr) option * stmt list
  (* WhileLoop: start-pos * pre-check * cond * body *)
  | WhileLoop of position * bool * expr * stmt list
  | Return of position * expr
  | ReturnVoid of position
  | TypeDecl of position * string * vartype * Types.visibility * bool
  | Label of position * string
  | Goto of position * string
  | Break of position
  | Continue of position
  | Sync of position

let operator2string = function
  | OperIncr -> "++"
  | OperDecr -> "--"
  | OperAddrOf -> "&"
  | OperMinus -> "-"
  | OperPlus -> "+"
  | OperLogicalNot -> "!"
  | OperBitwiseNot -> "~"
  | OperMult -> "*"
  | OperDiv -> "/"
  | OperRemainder -> "%"
  | OperLShift -> "<<"
  | OperRShift -> ">>"
  | OperLT -> "<"
  | OperGT -> ">"
  | OperLTE -> "<="
  | OperGTE -> ">="
  | OperEquals -> "=="
  | OperNEquals -> "!="
  | OperBitwiseAnd -> "&"
  | OperBitwiseXor -> "^"
  | OperBitwiseOr -> "|"
  | OperLogicalAnd -> "&&"
  | OperLogicalOr -> "||"
  | OperEllipsis -> "..."
  | OperAssign -> "="
  | OperPlusAssign -> "+="
  | OperMinusAssign -> "-="
  | OperMultAssign -> "*="
  | OperDivAssign -> "/="
  | OperRemAssign -> "%="
  | OperLShiftAssign -> "<<="
  | OperRShiftAssign -> ">>="
  | OperBAndAssign -> "&="
  | OperBXorAssign -> "^="
  | OperBOrAssign -> "|="

let literal2primitive = function
  | LitBool _ -> PrimBool
  | LitI8  _ -> PrimI8
  | LitU8  _ -> PrimU8
  | LitI16 _ -> PrimI16
  | LitU16 _ -> PrimU16
  | LitI32 _ -> PrimI32
  | LitU32 _ -> PrimU32
  | LitI64 _ -> PrimI64
  | LitU64 _ -> PrimU64
  | LitF32 _ -> PrimF32
  | LitF64 _ -> PrimF64

let of_parsetree =
  let rec get_documentation = function
    | [] -> None
    | str :: rest ->
       begin try let sub = String.sub str 0 3 in
                 if sub = "/**" then Some str else get_documentation rest
             with _ -> get_documentation rest
       end
  in
  let visdoc = function
    | None -> VisLocal, None
    | Some exported ->
       VisExported exported.td_pos, get_documentation exported.td_noncode
  in
  let void_rettp_p = function
    | PTT_Name { td_text = "void" } -> true
    | _ -> false
  in
  let unop_of = function
    | { td_text = "++" } -> OperIncr
    | { td_text = "--" } -> OperDecr
    | { td_text = "-" } -> OperMinus
    | { td_text = "+" } -> OperPlus
    | { td_text = "!" } -> OperLogicalNot
    | { td_text = "~" } -> OperBitwiseNot
    | { td_text = "&" } -> OperAddrOf
    | op -> Report.err_internal __FILE__ __LINE__
                                ("unknown unary operator: " ^ op.td_text)
  in
  let binop_of = function
    | { td_text = "*" } -> OperMult
    | { td_text = "/" } -> OperDiv
    | { td_text = "%" } -> OperRemainder
    | { td_text = "+" } -> OperPlus
    | { td_text = "-" } -> OperMinus
    | { td_text = "<<" } -> OperLShift
    | { td_text = ">>" } -> OperRShift
    | { td_text = "<" } -> OperLT
    | { td_text = ">" } -> OperGT
    | { td_text = "<=" } -> OperLTE
    | { td_text = ">=" } -> OperGTE
    | { td_text = "==" } -> OperEquals
    | { td_text = "!=" } -> OperNEquals
    | { td_text = "&" } -> OperBitwiseAnd
    | { td_text = "^" } -> OperBitwiseXor
    | { td_text = "|" } -> OperBitwiseOr
    | { td_text = "&&" } -> OperLogicalAnd
    | { td_text = "||" } -> OperLogicalOr
    | { td_text = "..." } -> OperEllipsis
    | { td_text = "=" } -> OperAssign
    | { td_text = "+=" } -> OperPlusAssign
    | { td_text = "-=" } -> OperMinusAssign
    | { td_text = "*=" } -> OperMultAssign
    | { td_text = "/=" } -> OperDivAssign
    | { td_text = "%=" } -> OperRemAssign
    | { td_text = "<<=" } -> OperLShiftAssign
    | { td_text = ">>=" } -> OperRShiftAssign
    | { td_text = "&=" } -> OperBAndAssign
    | { td_text = "^=" } -> OperBXorAssign
    | { td_text = "|=" } -> OperBOrAssign
    | op -> Report.err_internal __FILE__ __LINE__
                                ("unknown binary operator: " ^ op.td_text)
  in
  let rec stmt_of = function
    | PTS_Import (importtok, (pathtok, _), _) -> Import (importtok, pathtok)
    | PTS_Begin (b, stmts, _) -> Block (b.td_pos, List.map stmt_of stmts)
    | PTS_FcnDefExpr ((exp, def, id, template_maybe, tp), equals, e, _) ->
       let vis, doc = visdoc exp in
       let contents = if void_rettp_p tp then StmtExpr (def.td_pos, expr_of e)
                      else Return (equals.td_pos, expr_of e)
       in
       if template_maybe = None then
         DefFcn (def.td_pos, doc, vis, id.td_text, type_of tp, [contents])
       else
         DefTemplateFcn (def.td_pos, Util.the template_maybe, doc, vis,
                         id.td_text, type_of tp, [contents])
    | PTS_FcnDefBlock ((exp, def, id, template_maybe, tp), stmt) ->
       let vis, doc = visdoc exp in
       if template_maybe = None then
         DefFcn (def.td_pos, doc, vis, id.td_text, type_of tp, [stmt_of stmt])
       else
         DefTemplateFcn (def.td_pos, Util.the template_maybe, doc, vis,
                         id.td_text, type_of tp, [stmt_of stmt])
    | PTS_FcnDecl (decl, id, tp, _) ->
       DeclFcn (decl.td_pos, VisExported decl.td_pos, id.td_text, type_of tp)
    | PTS_Expr (e, _) ->
       StmtExpr (pt_expr_pos e, expr_of e)
    | PTS_Var (_, ids, tp, _) ->
       VarDecl (ids, [], type_of tp)
    | PTS_VarInit (var, ids, tp_opt, eq, elist, _) ->
       let asttp = if tp_opt = None then InferredType
                   else type_of (Util.the tp_opt) in
       let initify id expr =
         ExprBinary { op_pos = eq.td_pos;
                      op_op = OperAssign;
                      op_left = ExprVar (id.td_pos, id.td_text);
                      op_right = Some (expr_of expr);
                      op_atomic = false;
                    }
       in
       VarDecl (ids, List.map2 initify ids elist, asttp)
    | PTS_VarInlineStruct (var, _, vlist, _, _, e, _) ->
       let cvlist =
         List.map (fun (id, tp) -> id.td_pos, id.td_text, type_of tp) vlist
       in
       InlineStructVarDecl (var.td_pos, cvlist, (pt_expr_pos e, expr_of e))
    | PTS_DeleteExpr (d, e, _) ->
       let expr = ExprFcnCall { fc_pos = d.td_pos;
                                fc_name = "forkscan_free";
                                fc_template = [];
                                fc_args = [ expr_of e ];
                                fc_spawn = false
                              }
       in
       StmtExpr (d.td_pos, expr)
    | PTS_RetireExpr (r, e, _) ->
       let expr = ExprFcnCall { fc_pos = r.td_pos;
                                fc_name = "forkscan_retire";
                                fc_template = [];
                                fc_args = [ expr_of e ];
                                fc_spawn = false
                              }
       in
       StmtExpr (r.td_pos, expr)
    | PTS_XBegin (tok, _) -> XBegin tok.td_pos
    | PTS_XCommit (tok, _) -> XCommit tok.td_pos
    | PTS_IfStmt (iftok, cond, _, stmts, elifs, maybe_else, _) ->
       let else_clause = match maybe_else with
         | None -> None
         | Some (_, stmts) -> Some (List.map stmt_of stmts)
       in
       let rest = List.fold_right (fun (elif, c, _, stmts) rest ->
                      Some [ IfStmt (elif.td_pos, expr_of c,
                                      List.map stmt_of stmts,
                                      rest) ])
                           elifs else_clause
       in
       IfStmt (iftok.td_pos, expr_of cond, List.map stmt_of stmts, rest)
    | PTS_ForLoop (fortok, init_p, _, cond, _, iter_p, _, stmts, _)
    | PTS_ParforLoop (fortok, init_p, _, cond, _, iter_p, _, stmts, _)
      ->
       let init = match init_p with
         | None -> None
         | Some (PTForInit_Var (_, id, tp_opt, eq, e)) ->
            let init =
              ExprBinary { op_pos = eq.td_pos;
                           op_op = OperAssign;
                           op_left = ExprVar (id.td_pos, id.td_text);
                           op_right = Some (expr_of e);
                           op_atomic = false;
                         }
            in
            let tp = match tp_opt with
              | Some t -> type_of t
              | None -> InferredType
            in
            Some (VarDecl ([id], [init], tp))
         | Some (PTForInit_Expr e) ->
            Some (StmtExpr (pt_expr_pos e, expr_of e))
       in
       let iter = match iter_p with
         | None -> None
         | Some e -> Some (pt_expr_pos e, expr_of e)
       in
       let is_parallel = if fortok.td_text = "for" then false else true in
       ForLoop (fortok.td_pos, is_parallel, init,
                (pt_expr_pos cond, expr_of cond),
                iter, List.map stmt_of stmts)
    | PTS_WhileLoop (whiletok, cond, _, stmts, _) ->
       WhileLoop (whiletok.td_pos, true, expr_of cond,
                  List.map stmt_of stmts)
    | PTS_DoWhileLoop (dotok, stmts, _, _, cond, _) ->
       WhileLoop (dotok.td_pos, false, expr_of cond,
                  List.map stmt_of stmts)
    | PTS_SwitchStmt _ ->
       Report.err_internal __FILE__ __LINE__ "Switch not supported."
    | PTS_ReturnExpr (ret, e, _) ->
       Return (pt_expr_pos e, expr_of e)
    | PTS_Return (ret, _) ->
       ReturnVoid ret.td_pos
    | PTS_Type (vis_p, typedef, id, _, tp, _) ->
       let vis, opaque = match vis_p with
         | None -> VisLocal, false
         | Some (exp, None) -> VisExported exp.td_pos, false
         | Some (exp, Some _) -> VisExported exp.td_pos, true
       in
       TypeDecl (typedef.td_pos, id.td_text, type_of tp, vis, opaque)
    | PTS_Goto (goto, id, _) -> Goto (goto.td_pos, id.td_text)
    | PTS_Break (break, _) -> Break break.td_pos
    | PTS_Label (id, _) -> Label (id.td_pos, id.td_text)
    | PTS_Continue (continue, _) -> Continue continue.td_pos
    | PTS_Sync (sync, _) -> Sync sync.td_pos
  and type_of = function
    | PTT_Volatile (vol, tp) ->
       let ret = match type_of tp with
         | VarType (pos, nm, []) ->
            VarType (pos, nm, [ Volatile ])
         | PtrType (pos, tp, []) ->
            PtrType (pos, tp, [ Volatile ])
         | VarType _
         | PtrType _ -> Report.err_multiple_volatile_keywords vol.td_pos
         | _ ->
            Report.err_internal
              __FILE__ __LINE__
              "volatile not yet supported on non-var|ptr-types"
       in
       ret
    | PTT_Name id -> VarType (id.td_pos, id.td_text, [])
    | PTT_Ptr (star, tp) -> PtrType (star.td_pos, type_of tp, [])
    | PTT_Array (lsquare, Some e, _, tp) ->
       ArrayType (lsquare.td_pos, expr_of e, type_of tp)
    | PTT_Array (lsquare, None, rsquare, tp) ->
       let array_dim = ExprLit (rsquare.td_pos, LitI64 0L) in
       ArrayType (lsquare.td_pos, array_dim, type_of tp)
    | PTT_Struct (_, contents, _) ->
       StructType (List.map (fun (id, tp) ->
                       id.td_pos, id.td_text, type_of tp)
                            contents)
    | PTT_StructUnnamed (_, contents, _) ->
       StructType (List.map (fun tp ->
                       pt_type_pos tp, "", type_of tp)
                            contents)
    | PTT_Fcn (_, params, _, _, ret) ->
       let param_convert = function
         | PTP_Var (id, tp) -> id.td_pos, id.td_text, type_of tp
         | PTP_Type tp -> pt_type_pos tp, "", type_of tp
         | PTP_Ellipsis tok -> tok.td_pos, "", Ellipsis tok.td_pos
       in
       let cparams = List.map param_convert params in
       FcnType (cparams, type_of ret)
  and expr_of = function
    | PTE_New (newtok, tp, init_p) ->
       let init = match init_p with
         | None -> []
         | Some (_, field_inits, _) ->
            List.map (fun field ->
                match field.ptfi_array with
                | None ->
                   field.ptfi_fname.td_pos,
                   field.ptfi_fname.td_text,
                   None,
                   pt_expr_pos field.ptfi_expr,
                   expr_of field.ptfi_expr
                | Some (lsquare, e, _) ->
                   field.ptfi_fname.td_pos,
                   field.ptfi_fname.td_text,
                   Some (lsquare.td_pos, expr_of e),
                   pt_expr_pos field.ptfi_expr,
                   expr_of field.ptfi_expr
              )
                     field_inits
       in
       ExprNew (newtok.td_pos, type_of tp, init)
    | PTE_Nil nil -> ExprNil nil.td_pos
    | PTE_Type (typetok, tp) ->
       ExprType (typetok.td_pos, type_of tp)
    | PTE_I64 (tok, value) -> ExprLit (tok.td_pos, LitI64 value)
    | PTE_U64 (tok, value) -> ExprLit (tok.td_pos, LitU64 value)
    | PTE_I32 (tok, value) -> ExprLit (tok.td_pos, LitI32 value)
    | PTE_U32 (tok, value) -> ExprLit (tok.td_pos, LitU32 value)
    | PTE_I16 (tok, value) -> ExprLit (tok.td_pos, LitI16 value)
    | PTE_U16 (tok, value) -> ExprLit (tok.td_pos, LitU16 value)
    | PTE_I8 (tok, value) -> ExprLit (tok.td_pos, LitI8 value)
    | PTE_U8 (tok, value) -> ExprLit (tok.td_pos, LitU8 value)
    | PTE_Bool (tok, value) -> ExprLit (tok.td_pos, LitBool value)
    | PTE_F64 (tok, value) -> ExprLit (tok.td_pos, LitF64 value)
    | PTE_F32 (tok, value) -> ExprLit (tok.td_pos, LitF32 value)
    | PTE_String (tok, value) -> ExprString (tok.td_pos, value)
    | PTE_FcnCall fcn ->
       let template = match fcn.ptfc_template with
         | None -> []
         | Some tinst ->
            let get_template_name = function
              | PTT_Name tok -> tok
              | _ -> Report.err_internal __FILE__ __LINE__
                                         "Unexpected template type."
            in
            List.map get_template_name tinst.ptti_args
       in
       let fcn_call = { fc_pos = fcn.ptfc_name.td_pos;
                        fc_name = fcn.ptfc_name.td_text;
                        fc_template = template;
                        fc_args = List.map expr_of fcn.ptfc_args;
                        fc_spawn =
                          if fcn.ptfc_spawn = None then false else true
                      }
       in
       ExprFcnCall fcn_call
    | PTE_Var var ->
       ExprVar (var.td_pos, var.td_text)
    | PTE_StaticStruct (lcurly, fields, _) ->
       ExprStaticStruct (lcurly.td_pos,
                         List.map (fun e -> pt_expr_pos e, expr_of e) fields)
    | PTE_StaticArray (lsquare, elements, _) ->
       ExprStaticArray (lsquare.td_pos,
                        List.map (fun e -> pt_expr_pos e, expr_of e) elements)
    | PTE_Index (base, _, idx, _) ->
       ExprIndex (pt_expr_pos base,
                  expr_of base,
                  pt_expr_pos idx,
                  expr_of idx)
    | PTE_SelectField (obj, dot, field) ->
       ExprSelectField (dot.td_pos,
                        field.td_pos,
                        expr_of obj,
                        FieldName field.td_text)
    | PTE_PostUni (e, op) ->
       let oper = { op_pos = op.td_pos;
                    op_op = unop_of op;
                    op_left = expr_of e;
                    op_right = None;
                    op_atomic = false;
                  }
       in
       ExprPostUnary oper
    | PTE_PreUni (op, e) ->
       let oper = { op_pos = op.td_pos;
                    op_op = unop_of op;
                    op_left = expr_of e;
                    op_right = None;
                    op_atomic = false;
                  }
       in
       ExprPreUnary oper
    | PTE_Bin (left, atomic_opt, op, right) ->
       (* FIXME: WORKING HERE!  Carry atomic_opt into the AST. *)
       let oper = { op_pos = op.td_pos;
                    op_op = binop_of op;
                    op_left = expr_of left;
                    op_right = Some (expr_of right);
                    op_atomic = if atomic_opt = None then false
                                else true
                  }
       in
       ExprBinary oper
  in
  List.map stmt_of

let rec pos_of_astexpr = function
  | ExprNew (pos, _, _)
  | ExprFcnCall { fc_pos = pos }
  | ExprString (pos, _)
  | ExprPreUnary { op_pos = pos }
  | ExprPostUnary { op_pos = pos }
  | ExprVar (pos, _)
  | ExprLit (pos, _)
  | ExprCast (pos, _, _)
  | ExprIndex (pos, _, _, _)
  | ExprSelectField (pos, _, _, _)
  | ExprStaticStruct (pos, _)
  | ExprStaticArray (pos, _)
  | ExprType (pos, _)
  | ExprTypeString (pos, _)
  | ExprNil pos ->
     pos
  | ExprBinary { op_left = operand } ->
     pos_of_astexpr operand

(* Visit all subexpressions and call the function on them. *)
let visit_expr f =
  let rec visit expr =
    f expr;
    match expr with
    | ExprNew (_, _, init) ->
       let visit_init (_, _, p_e_opt, _, expr) =
         visit expr;
         match p_e_opt with
         | None -> ()
         | Some (_, e) -> visit e
       in
       List.iter visit_init init
    | ExprFcnCall fcn_call -> List.iter visit fcn_call.fc_args
    | ExprString _ -> ()
    | ExprBinary op ->
       begin
         visit op.op_left;
         visit (Util.the op.op_right)
       end
    | ExprPreUnary op
    | ExprPostUnary op -> visit op.op_left
    | ExprVar _ -> ()
    | ExprLit _ -> ()
    | ExprCast (_, _, e) -> visit e
    | ExprIndex (_, base, _, idx) ->
       begin
         visit base;
         visit idx
       end
    | ExprSelectField (_, _, e, _) -> visit e
    | ExprStaticStruct (_, members) ->
       List.iter (fun (_, e) -> visit e) members
    | ExprStaticArray (_, elements) ->
       List.iter (fun (_, e) -> visit e) elements
    | ExprType _ -> ()
    | ExprTypeString (_, e) -> visit e
    | ExprNil _ -> ()
  in
  visit

(* Visit all subexpressions in the stmt and call the function on them. *)
let rec visit_expr_in_stmt f = function
  | Import _ -> ()
  | StmtExpr (_, expr) -> visit_expr f expr
  | Block (_, block) -> List.iter (visit_expr_in_stmt f) block
  | DefFcn (_, _, _, _, _, body) -> List.iter (visit_expr_in_stmt f) body
  | DeclFcn _
  | DefTemplateFcn _ ->
     Report.err_internal __FILE__ __LINE__ "not implemented."
  | VarDecl (_, exprs, _) -> List.iter (visit_expr f) exprs
  | InlineStructVarDecl (_, _, (_, expr)) -> visit_expr f expr
  | XBegin _ -> ()
  | XCommit _ -> ()
  | IfStmt (_, cond, then_stmts, else_stmts_maybe) ->
     begin
       visit_expr f cond;
       List.iter (visit_expr_in_stmt f) then_stmts;
       if None <> else_stmts_maybe then
         List.iter (visit_expr_in_stmt f) (Util.the else_stmts_maybe)
     end
  | ForLoop (_, _, init_maybe, (_, cond), iter_option, body) ->
     begin
       if None <> init_maybe then
         visit_expr_in_stmt f (Util.the init_maybe);
       visit_expr f cond;
       List.iter (visit_expr_in_stmt f) body;
       match iter_option with
       | None -> ()
       | Some (_, e) -> visit_expr f e
     end
  | WhileLoop (_, _, cond, body) ->
     begin
       visit_expr f cond;
       List.iter (visit_expr_in_stmt f) body
     end
  | Return (_, e) -> visit_expr f e
  | ReturnVoid _ -> ()
  | TypeDecl _ -> ()
  | Label _
  | Goto _
  | Break _
  | Continue _
  | Sync _ -> ()
