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

open Cimportext
open Isminterp
open Lexing
open Operator
open Parsetree
open Types
open Util

type position = Lexing.position

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

type code_relation =
  | CRExpr of pt_expr
  | CRApproximate of position

and operation =
  { op_pos : position;
    op_op : Operator.t;
    op_left : expr;
    op_right : expr option;
    op_atomic : bool
  }

and field_id =
  | FieldNumber of int
  | FieldName of string

and expr =
  { expr_cr   : code_relation;
    expr_tp   : Types.deftype;
    expr_ast  : ast_expr
  }

and ast_expr =
  | ExprIsm of Parsetree.ism
  | ExprNew of (*array dim=*)expr * Types.deftype
               * (Parsetree.tokendata * expr) list
  | ExprFcnCall of string * expr list * (*spawn=*)bool
  | ExprBuiltinCall of string * expr list
  | ExprString of string
  | ExprBinary of operation
  | ExprPreUnary of operation
  | ExprPostUnary of operation
  | ExprTernaryCond of expr * expr * expr
  | ExprVar of string
  | ExprLit of literal
  | ExprEnum of string * literal
  | ExprCast of (*from=*)Types.deftype * (*to=*)Types.deftype * expr
  | ExprIndex of (*base=*)expr * (*idx=*)expr
  | ExprSelectField of expr * field_id
  | ExprStaticStruct of (*is_packed=*)bool * expr list
  | ExprStaticArray of expr list
  | ExprType of Types.deftype
  | ExprTypeString of expr
  | ExprNil
  | ExprWildcard

type stmt =
  | MultiStmt of stmt list
  | Import of Parsetree.tokendata * Parsetree.tokendata
  | StmtExpr of position * expr
  | Block of position * stmt list
  | DeclFcn of position * Types.visibility * string * Types.deftype
               * (position * string) list
  | DefFcn of position * string option * Types.visibility * string
              * Types.deftype
              * (position * string) list
              * stmt list
  | VarDecl of Parsetree.tokendata * Parsetree.tokendata list
               * expr list * Types.deftype * Types.visibility
  | InlineStructVarDecl of Parsetree.tokendata
                           * (position * string * Types.deftype) list
                           * (position * expr)
  | TransactionBlock of position * stmt list
                        * (Parsetree.tokendata * expr * stmt list) list
  | IfStmt of position * expr * stmt list * (position * stmt list) option
  (* ForLoop: start-pos * is_parallel * init * cond * iter * body *)
  | ForLoop of position * bool * stmt option * (position * expr)
    * (position * expr) option * position * stmt list
  (* WhileLoop: start-pos * pre-check * cond * body *)
  | WhileLoop of position * bool * expr * stmt list
  (* SwitchStmt:
     start-pos * expr * (case-pos * fall-through? * expr * body) list *)
  | SwitchStmt of position * expr * (position * bool * expr * stmt list) list
  | Return of position * expr
  | ReturnVoid of position
  | TypeDecl of position * string * Types.deftype * Types.visibility * bool
  | Label of position * string
  | Goto of position * string
  | Break of position
  | Continue of position
  | Sync of position

let faux_var =
  { td_pos = Util.faux_pos;
    td_text = "var";
    td_noncode = []
  }

let faux_global =
  { td_pos = Util.faux_pos;
    td_text = "global";
    td_noncode = []
  }

(* Basic types. *)
let inferred_base = DefTypeUnresolved ""
let inferred_type = maketype (Some faux_pos) inferred_base
let void_type = maketype (Some faux_pos) DefTypeVoid
let i1_type = maketype (Some faux_pos) (DefTypePrimitive PrimBool)
let i8_type = maketype (Some faux_pos) (DefTypePrimitive PrimI8)
let u8_type = maketype (Some faux_pos) (DefTypePrimitive PrimU8)
let i16_type = maketype (Some faux_pos) (DefTypePrimitive PrimI16)
let u16_type = maketype (Some faux_pos) (DefTypePrimitive PrimU16)
let i32_type = maketype (Some faux_pos) (DefTypePrimitive PrimI32)
let u32_type = maketype (Some faux_pos) (DefTypePrimitive PrimU32)
let i64_type = maketype (Some faux_pos) (DefTypePrimitive PrimI64)
let u64_type = maketype (Some faux_pos) (DefTypePrimitive PrimU64)
let f32_type = maketype (Some faux_pos) (DefTypePrimitive PrimF32)
let f64_type = maketype (Some faux_pos) (DefTypePrimitive PrimF64)
let wildcard_type = maketype (Some faux_pos) DefTypeWildcard
let nil_type = maketype (Some faux_pos) DefTypeNullPtr

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

let params_of = function
  | PTT_Fcn (_, params, _, _, _) ->
     let get_pair = function
       | PTP_Var (tok, _) -> tok.td_pos, tok.td_text
       | PTP_Type tp -> pt_type_pos tp, ""
       | PTP_Ellipsis tok -> tok.td_pos, ""
     in
     List.map get_pair params
  | _ ->
     Report.err_internal __FILE__ __LINE__
                         "trying to get params from non-function type."

let make_expr e tp astexpr =
  { expr_cr = CRExpr e;
    expr_tp = tp;
    expr_ast = astexpr
  }

let make_inserted_expr e tp astexpr =
  { expr_cr = CRApproximate (pt_expr_pos e);
    expr_tp = tp;
    expr_ast = astexpr
  }

let of_parsetree bindings =

  let builtin_types =
    let builtins = Hashtbl.create 32 in
    let add_type (name, tp, _, _, _, _) =
      Hashtbl.add builtins name tp
    in
    List.iter add_type map_builtin_types;
    builtins
  in

  let lookup_builtin_type pos name =
    try let t = Hashtbl.find builtin_types name in
        { dtpos = Some pos;
          bare = t.bare;
          dtvolatile = false
        }
    with _ -> maketype (Some pos) @@ DefTypeNamed name
  in

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
  let rec stmt_of = function
    | PTS_ISM_Stmts stmts -> MultiStmt (List.map stmt_of stmts)
    | PTS_Import (importtok, (pathtok, _), _) -> Import (importtok, pathtok)
    | PTS_Begin (b, stmts, _) -> Block (b.td_pos, List.map stmt_of stmts)
    | PTS_FcnDefExpr ((exp, def, id, tp), equals, e, _) ->
       let vis, doc = visdoc exp in
       let contents = if void_rettp_p tp then StmtExpr (def.td_pos, expr_of e)
                      else Return (equals.td_pos, expr_of e)
       in
       let idtok = tok_of_ident bindings id in
       DefFcn (def.td_pos, doc, vis, idtok.td_text, deftype_of tp,
               params_of tp, [contents])
    | PTS_FcnDefBlock ((exp, def, id, tp), stmt) ->
       let vis, doc = visdoc exp in
       let idtok = tok_of_ident bindings id in
       DefFcn (def.td_pos, doc, vis, idtok.td_text, deftype_of tp,
               params_of tp, [stmt_of stmt])
    | PTS_FcnDecl (decl, id, tp, _) ->
       DeclFcn (decl.td_pos, VisExported decl.td_pos, id.td_text,
                deftype_of tp, params_of tp)
    | PTS_Expr (e, _) ->
       StmtExpr (pt_expr_pos e, expr_of e)
    | PTS_Var (decl, ids, tp, _) ->
       VarDecl (decl, ids, [], deftype_of tp, VisLocal)
    | PTS_VarInit (decl, ids, tp_opt, eq, elist, _) ->
       let asttp = if tp_opt = None then
                     maketype (Some decl.td_pos) inferred_base
                   else deftype_of (Util.the tp_opt) in
       let initify id expr =
         let v = make_inserted_expr expr asttp (ExprVar id.td_text) in
         let ast = ExprBinary { op_pos = eq.td_pos;
                                op_op = OperAssign;
                                op_left = v;
                                op_right = Some (expr_of expr);
                                op_atomic = false;
                              }
         in
         make_expr expr asttp ast
       in
       VarDecl (decl, ids, List.map2 initify ids elist, asttp, VisLocal)
    | PTS_VarInlineStruct (decl, _, vlist, _, _, e, _) ->
       let cvlist =
         List.map (fun (id, tp) -> id.td_pos, id.td_text, deftype_of tp) vlist
       in
       InlineStructVarDecl (decl, cvlist, (pt_expr_pos e, expr_of e))
    | PTS_VarInlineStructInferred (decl, _, vlist, _, _, e, _) ->
       let cvlist =
         List.map
           (fun id ->
             id.td_pos,
             id.td_text,
             maketype (Some id.td_pos) inferred_base)
           vlist
       in
       InlineStructVarDecl (decl, cvlist, (pt_expr_pos e, expr_of e))
    | PTS_DeleteExpr (d, e, _) ->
       let ast = ExprFcnCall ("forkscan_free", [ expr_of e ], false) in
       StmtExpr (d.td_pos, make_inserted_expr e void_type ast)
    | PTS_RetireExpr (r, e, _) ->
       let ast = ExprFcnCall ("forkscan_retire", [ expr_of e ], false) in
       StmtExpr (r.td_pos, make_inserted_expr e void_type ast)
    | PTS_Transaction (atomic, xbegin, stmts, failclauses, xend) ->
       let clausify (ftok, pattern, _, stmts) =
         (ftok, expr_of pattern, List.map stmt_of stmts)
       in
       TransactionBlock (atomic.td_pos,
                         List.map stmt_of stmts,
                         List.map clausify failclauses)
    | PTS_IfStmt (iftok, cond, _, stmts, elifs, maybe_else, _) ->
       let else_clause = match maybe_else with
         | None -> None
         | Some (etok, stmts) -> Some (etok.td_pos, (List.map stmt_of stmts))
       in
       let rest = List.fold_right (fun (elif, c, _, stmts) rest ->
                      Some (elif.td_pos,
                            [ IfStmt (elif.td_pos, expr_of c,
                                      List.map stmt_of stmts,
                                      rest) ]))
                                  elifs else_clause
       in
       IfStmt (iftok.td_pos, expr_of cond, List.map stmt_of stmts, rest)
    | PTS_ForLoop (fortok, init_p, _, cond, _, iter_p, dotok, stmts, _)
    | PTS_ParforLoop (fortok, init_p, _, cond, _, iter_p, dotok, stmts, _)
      ->
       let init = match init_p with
         | None -> None
         | Some (PTForInit_Var (var, id, tp_opt, eq, e)) ->
            let tp = match tp_opt with
              | Some t -> deftype_of t
              | None -> maketype (Some var.td_pos) inferred_base
            in
            let init =
              let v = make_inserted_expr e tp (ExprVar id.td_text) in
              ExprBinary { op_pos = eq.td_pos;
                           op_op = OperAssign;
                           op_left = v;
                           op_right = Some (expr_of e);
                           op_atomic = false;
                         }
            in
            let astexpr = make_expr e tp init in
            Some (VarDecl (faux_var, [id], [astexpr], tp, VisLocal))
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
                iter, dotok.td_pos, List.map stmt_of stmts)
    | PTS_WhileLoop (whiletok, cond, _, stmts, _) ->
       WhileLoop (whiletok.td_pos, true, expr_of cond,
                  List.map stmt_of stmts)
    | PTS_DoWhileLoop (dotok, stmts, _, _, cond, _) ->
       WhileLoop (dotok.td_pos, false, expr_of cond,
                  List.map stmt_of stmts)
    | PTS_SwitchStmt (switchtok, expr, _, cases, _) ->
       let convert_case = function
         | PTMatchCase (_, ctor, _, stmts) ->
            pt_expr_pos ctor, false, expr_of ctor, List.map stmt_of stmts
         | PTFallCase (_, ctor, _, stmts) ->
            pt_expr_pos ctor, true, expr_of ctor, List.map stmt_of stmts
       in
       SwitchStmt (switchtok.td_pos, expr_of expr, List.map convert_case cases)
    | PTS_ReturnExpr (ret, e, _) ->
       Return (pt_expr_pos e, expr_of e)
    | PTS_Return (ret, _) ->
       ReturnVoid ret.td_pos
    | PTS_Type (vis_p, typedef, id, tp_opt, _) ->
       let vis, opaque = match vis_p with
         | None -> VisLocal, false
         | Some (exp, None) -> VisExported exp.td_pos, false
         | Some (exp, Some _) -> VisExported exp.td_pos, true
       in
       let tp = match tp_opt with
         | None -> maketype (Some id.td_pos) (DefTypeOpaque id.td_text)
         | Some (_, t) -> deftype_of t
       in
       TypeDecl (typedef.td_pos, id.td_text, tp, vis, opaque)
    | PTS_Goto (goto, id, _) -> Goto (goto.td_pos, id.td_text)
    | PTS_Break (break, _) -> Break break.td_pos
    | PTS_Label (id, _) -> Label (id.td_pos, id.td_text)
    | PTS_Continue (continue, _) -> Continue continue.td_pos
    | PTS_Sync (sync, _) -> Sync sync.td_pos

  and eval_array_dim = function
    | PTE_I64 (_, n) | PTE_U64 (_, n) -> n
    | PTE_I32 (_, n) | PTE_U32 (_, n)
    | PTE_I16 (_, n) | PTE_U16 (_, n) -> Int64.of_int32 n
    | PTE_I8  (_, c) | PTE_U8  (_, c) -> Int64.of_int @@ Char.code c
    | PTE_F32 (tok, _) | PTE_F64 (tok, _) ->
       Report.err_float_array_dim tok.td_pos
    | expr ->
       (* The dimension is too complex to resolve, here.  It may, in fact,
          be determined at runtime, if this is a "new" statement. *)
       -1L

  and deftype_of = function
    | PTT_Fcn (lparen, params, _, _, ret) ->
       let rec param_convert accum = function
         | [] -> List.rev accum, false
         | PTP_Var (_, parse_tp) :: rest
         | PTP_Type parse_tp :: rest ->
            let tp = deftype_of parse_tp in
            param_convert (tp :: accum) rest
         | [ PTP_Ellipsis tok ] ->
            List.rev accum, true
         | PTP_Ellipsis tok :: _ ->
            Report.err_vararg_not_last tok.td_pos
       in
       let params, is_variadic = param_convert [] params in
       let base = DefTypeFcn (params, deftype_of ret, is_variadic) in
       maketype (Some lparen.td_pos) base
    | PTT_Volatile (vol, tp) -> volatile_of @@ deftype_of tp
    | PTT_Name id ->
       lookup_builtin_type id.td_pos id.td_text
    | PTT_Ptr (star, tp) ->
       maketype (Some star.td_pos) (DefTypePtr (deftype_of tp))
    | PTT_Array (lsquare, size_expr_opt, _, tp) ->
       let sz = if size_expr_opt = None then 0L
                else eval_array_dim @@ Util.the size_expr_opt
       in
       (* FIXME: The array dimension *should* be int64.  This is ridiculous. *)
       let szi = Int64.to_int sz in
       maketype (Some lsquare.td_pos) (DefTypeArray (deftype_of tp, szi))
    | PTT_Struct (packed_maybe, lcurly, name_tp_pairs, _) ->
       let is_packed = match packed_maybe with
         | Some _ -> true
         | None -> false
       in
       let el_list = List.map (fun (n, t) -> deftype_of t, n.td_text)
                              name_tp_pairs
       in
       let types, names = List.split el_list in
       maketype (Some lcurly.td_pos)
         (DefTypeLiteralStruct (is_packed, types, names))
    | PTT_StructUnnamed (packed_maybe, lcurly, tp_list, _) ->
       let is_packed = match packed_maybe with
         | Some _ -> true
         | None -> false
       in
       let el_list = List.map (fun t -> deftype_of t, "") tp_list in
       let types, names = List.split el_list in
       maketype (Some lcurly.td_pos)
         (DefTypeLiteralStruct (is_packed, types, names))
    | PTT_Enum (enum, alts) ->
       let alt_names = List.map (fun tok -> tok.td_text) alts in
       maketype (Some enum.td_pos) (DefTypeEnum alt_names)

  and expr_of e =
    match e with
    | PTE_IsmExpr (at, sexpr) ->
       let ast = ExprIsm sexpr in
       make_expr e inferred_type ast
    | PTE_New (newtok, tp, init_p) ->
       let init = match init_p with
         | None -> []
         | Some (_, field_inits, _) ->
            List.map (fun field -> field.ptfi_fname,
                                   expr_of field.ptfi_expr) field_inits
       in
       let array_pos = pt_type_pos tp in
       let rettp, deftp, array_dim = match tp with
         | PTT_Array (_, None, _, _) ->
            Report.err_cant_resolve_array_dim array_pos
         | PTT_Array (_, Some expr, _, subtype) ->
            let deftp = deftype_of subtype in
            makeptr deftp, deftp, expr_of expr
         | _ ->
            let deftp = deftype_of tp in
            makeptr deftp,
            deftp,
            make_expr e i64_type (ExprLit (LitI64 1L))
       in
       let ast = ExprNew (array_dim, deftp, init) in
       make_expr e rettp ast
    | PTE_Nil nil -> make_expr e nil_type ExprNil
    | PTE_Type (typetok, tp) ->
       make_expr e inferred_type (ExprType (deftype_of tp))
    | PTE_I64 (tok, value) -> make_expr e i64_type (ExprLit (LitI64 value))
    | PTE_U64 (tok, value) -> make_expr e u64_type (ExprLit (LitU64 value))
    | PTE_I32 (tok, value) -> make_expr e i32_type (ExprLit (LitI32 value))
    | PTE_U32 (tok, value) -> make_expr e u32_type (ExprLit (LitU32 value))
    | PTE_I16 (tok, value) -> make_expr e i16_type (ExprLit (LitI16 value))
    | PTE_U16 (tok, value) -> make_expr e u16_type (ExprLit (LitU16 value))
    | PTE_I8 (tok, value) -> make_expr e i8_type (ExprLit (LitI8 value))
    | PTE_U8 (tok, value) -> make_expr e u8_type (ExprLit (LitU8 value))
    | PTE_Bool (tok, value) -> make_expr e i1_type (ExprLit (LitBool value))
    | PTE_F64 (tok, value) -> make_expr e f64_type (ExprLit (LitF64 value))
    | PTE_F32 (tok, value) -> make_expr e f32_type (ExprLit (LitF32 value))
    | PTE_String (tok, value) ->
       let bare_array = DefTypeArray (i8_type, String.length value) in
       make_expr e (maketype None bare_array) (ExprString value)
    | PTE_Wildcard tok -> make_expr e wildcard_type ExprWildcard
    | PTE_FcnCall fcn ->
       let ast = ExprFcnCall (fcn.ptfc_name.td_text,
                              List.map expr_of fcn.ptfc_args,
                              if fcn.ptfc_spawn = None then false else true)
       in
       make_expr e inferred_type ast
    | PTE_Cast (_, tp, _, expr, _) ->
       let deftp = deftype_of tp in
       let defexpr = expr_of expr in
       let ast = ExprCast (inferred_type, deftp, defexpr) in
       make_expr e deftp ast
    | PTE_Var var ->
       make_expr e inferred_type (ExprVar var.td_text)
    | PTE_StaticStruct (packed_maybe, lcurly, fields, _) ->
       let is_packed = match packed_maybe with
         | None -> false
         | Some _ -> true
       in
       let fields = List.map (fun e -> expr_of e) fields in
       let ast = ExprStaticStruct (is_packed, fields) in
       make_expr e inferred_type ast
    | PTE_StaticArray (lsquare, elements, _) ->
       let ast = ExprStaticArray (List.map (fun e -> expr_of e) elements) in
       make_expr e inferred_type ast
    | PTE_Index (base, _, idx, _) ->
       let ast = ExprIndex (expr_of base, expr_of idx) in
       make_expr e inferred_type ast
    | PTE_SelectField (obj, dot, PT_FieldString field) ->
       let ast = ExprSelectField (expr_of obj, FieldName field.td_text) in
       make_expr e inferred_type ast
    | PTE_SelectField (obj, dot, PT_FieldInt (_, _, field_n, _)) ->
       let ast = ExprSelectField (expr_of obj,
                                  FieldNumber (Int32.to_int field_n))
       in
       make_expr e inferred_type ast
    | PTE_PostUni (sub, (unop, op)) ->
       let oper = { op_pos = op.td_pos;
                    op_op = unop;
                    op_left = expr_of sub;
                    op_right = None;
                    op_atomic = false;
                  }
       in
       make_expr e oper.op_left.expr_tp (ExprPostUnary oper)
    | PTE_PreUni ((unop, op), sub) ->
       let oper = { op_pos = op.td_pos;
                    op_op = unop;
                    op_left = expr_of sub;
                    op_right = None;
                    op_atomic = false;
                  }
       in
       make_expr e oper.op_left.expr_tp (ExprPreUnary oper)
    | PTE_Bin (left, atomic_opt, (binop, op), right) ->
       let oper = { op_pos = op.td_pos;
                    op_op = binop;
                    op_left = expr_of left;
                    op_right = Some (expr_of right);
                    op_atomic = if atomic_opt = None then false
                                else true
                  }
       in
       make_expr e inferred_type (ExprBinary oper)
    | PTE_TernaryCond (pt_cond, _, pt_left, _, pt_right) ->
       let cond = expr_of pt_cond
       and left = expr_of pt_left
       and right = expr_of pt_right in
       make_expr e inferred_type (ExprTernaryCond (cond, left, right))
  in
  List.map stmt_of

let of_cimport cimport =

  let builtin_ctypes =
    let builtins = Hashtbl.create 64 in
    let add_type (_, dt, _, ct_list, _, _) =
      List.iter (fun ct -> Hashtbl.add builtins ct dt) ct_list
    in
    List.iter add_type map_builtin_types;
    builtins
  in

  let lookup_builtin_ctype pos name =
    try let t = Hashtbl.find builtin_ctypes name in
        let ret =
          { dtpos = Some pos;
            bare = t.bare;
            dtvolatile = false
          }
        in
        ret, ret
    with _ ->
      maketype (Some pos) @@ DefTypeNamed name,
      maketype (Some pos) @@ DefTypeOpaque name
  in

  let nonbasic_regex = Str.regexp {|^\(union \)\|\(struct \)\|\(enum \)|} in
  let no_duplicates = Hashtbl.create 16 in
  let sanitize_ident name =
    match String.split_on_char ' ' name with
    | "struct" :: rest
    | "union" :: rest
    | "enum" :: rest
    | rest ->
       String.concat " " rest
  in

  let get_lval (lval, rval) = lval
  and get_rval (lval, rval) = rval
  in

  let deftype_of t =
    let rec deptr_fcns rawtype =
      let replace_raw bare =
        { dtpos = rawtype.dtpos;
          bare = bare;
          dtvolatile = rawtype.dtvolatile
        }
      in
      match rawtype.bare with
      | DefTypeFcn (rawparams, rawret, is_variadic) ->
         let params = List.map deptr_fcns rawparams in
         let ret = deptr_fcns rawret in
         replace_raw @@ DefTypeFcn (params, ret, is_variadic)
      | DefTypePtr ({ bare = DefTypeFcn _ } as subtype) ->
         (* The "interesting" case, where we eliminate a redundant
            function pointer. *)
         deptr_fcns subtype
      | DefTypeArray (subtype, n) ->
         replace_raw @@ DefTypeArray (deptr_fcns subtype, n)
      | DefTypeLiteralStruct (is_packed, raw_tplist, names) ->
         let tplist = List.map deptr_fcns raw_tplist in
         replace_raw @@ DefTypeLiteralStruct (is_packed, tplist, names)
      | DefTypeLiteralUnion (raw_tplist, names) ->
         let tplist = List.map deptr_fcns raw_tplist in
         replace_raw @@ DefTypeLiteralUnion (tplist, names)
      | _ -> rawtype
    in
    let rec dt_of = function
      | CT_TypeName (pos, tp) ->
         begin
           match sanitize_ident tp with
           | "__builtin_va_list" ->
              let ret = maketype (Some pos) DefTypeVAList in ret, ret
           | name when Str.string_match nonbasic_regex tp 0 ->
              (* Non-basic type: struct/union/enum.  Make sure there are no
                 duplicates. *)
              begin
                maketype (Some pos) @@ DefTypeNamed name,
                try Hashtbl.find no_duplicates name
                with _ ->
                  (* Note: Insert "Unresolved" type.  C allows its users to
                     specify undefined structs as, e.g., parameters to
                     functions.  It will get treated as an opaque type when
                     used through a pointer, but it hasn't (yet) been
                     typedef'd.  The C compiler doesn't care because... a
                     pointer is a pointer.

                     If it isn't through a pointer, and is never defined, then
                     the C compiler has already generated an error, and DEF
                     doesn't have to.

                     The same is true for fields in a struct or union.  If the
                     reference isn't a pointer, then rest assured, we'll get
                     the real definition eventually.
                   *)
                  let rval = maketype (Some pos) @@ DefTypeUnresolved "hello" in
                  let () = Hashtbl.replace no_duplicates name rval in
                  rval
              end
           | name ->
              lookup_builtin_ctype pos name
         end
      | CT_Pointer (pos, tp) ->
         let ret = maketype (Some pos) @@ DefTypePtr (get_lval (dt_of tp)) in
         ret, ret
      | CT_Record (kind, fields) ->
         let field_convert (_, name, ctype) = name, get_lval (dt_of ctype) in
         let names, types = List.split @@ List.map field_convert fields in
         let (pos, _, _) = List.hd fields in
         let bare = match kind with
           (* FIXME: Need to capture if the type is packed. *)
           | CR_Struct -> DefTypeLiteralStruct (false, types, names)
           | CR_Union -> DefTypeLiteralUnion (types, names)
         in
         let ret = maketype (Some pos) bare in
         ret, ret
      | CT_Function (pos, cparams, is_variadic, cret) ->
         let params = List.map (fun p -> get_lval (dt_of p)) cparams in
         let rettp = get_lval @@ dt_of cret in
         let ret =
           maketype (Some pos) @@ DefTypeFcn (params, rettp, is_variadic)
         in
         ret, ret
      | CT_Array (pos, csubtype, sz) ->
         let sub = get_lval @@ dt_of csubtype in
         let ret = maketype (Some pos) @@ DefTypeArray (sub, sz) in
         ret, ret
    in
    let lval, rval = dt_of t in
    deptr_fcns lval, deptr_fcns rval
  in
  let cparams_of params =
    let pair_of = function
      | CT_TypeName (pos, _) -> pos, ""
      | CT_Pointer (pos, _) -> pos, ""
      | CT_Record (_, members) ->
         let pos, _, _ = List.hd members in pos, ""
      | CT_Function (pos, _, _, _) -> pos, ""
      | CT_Array (pos, _, _) -> pos, ""
    in
    List.map pair_of params
  in

  let rec convert accum = function
    | [] -> List.rev accum
    | CV_Function (pos, name, params, is_variadic, ret) :: rest ->
       let ret_tp = get_lval (deftype_of ret) in
       let ast_params = List.map (fun p -> get_lval (deftype_of p)) params in
       let bare = DefTypeFcn (ast_params, ret_tp, is_variadic) in
       let ftype = maketype (Some pos) bare in
       let decl = DeclFcn (pos, Types.VisExternal, name, ftype,
                           (cparams_of params)
                           @ if is_variadic then [pos, ""] else []) in
       convert (decl :: accum) rest
    | CV_Typedecl (pos, name, maybe_tp) :: rest ->
       let name = sanitize_ident name in
       let decl_n_accum =
         try
           begin match Hashtbl.find no_duplicates name with
           | { bare = DefTypeUnresolved _ } ->
              let tp = match maybe_tp with
                | None -> maketype (Some pos) @@ DefTypeOpaque name
                | Some t -> get_rval (deftype_of t)
              in
              let () = Hashtbl.replace no_duplicates name tp in
              let decl = TypeDecl (pos, name, tp, VisLocal, false) in
              decl :: accum
           | { bare = DefTypeOpaque _ } ->
              let tp = match maybe_tp with
                | None -> Report.err_internal __FILE__ __LINE__
                                              "Redefined type."
                | Some t -> get_rval (deftype_of t)
              in
              let () = Hashtbl.replace no_duplicates name tp in
              let decl = TypeDecl (pos, name, tp, VisLocal, false) in
              decl :: accum
           | _ ->
              (* This is a strange case - that the struct has already been
                 defined and it seems to be redefined.  It's an artifact of
                 the way DEF uses Clang to parse C files.

                 When DEF calls Clang, Clang creates its own AST, and when
                 DEF traverses it (to bring it here), it will often ask for
                 definitions of, e.g., structs at the point of forward-
                 declaration.  Clang knows the definition, so it gives them
                 back to DEF.  DEF later comes across the actual point of
                 definition and apparently "redefines" the struct.

                 If the struct had _actually_ been redefined, Clang would have
                 caught it.  So we just ignore the apparent redefinition, here.
               *)
              accum
           end
         with _ ->
           let tp =
             match maybe_tp with
             | None -> maketype (Some pos) @@ DefTypeOpaque name
             | Some t ->
                match get_rval (deftype_of t) with
                | { dtpos = dtpos;
                    bare = DefTypeUnresolved str
                  } ->
                   (* This is weird, right?  Like we've made a type and it
                      came back "unresolved"?  Well, sometimes people make
                      opaque types with:

                        typedef struct foo foo;

                      If "struct foo" was never defined, typedef'ing foo
                      gives us an uresolved problem.  This catches that.
                    *)
                   { dtpos = dtpos;
                     bare = DefTypeOpaque str;
                     dtvolatile = false
                   }
                | rval -> rval
           in
           let () = Hashtbl.replace no_duplicates name tp in
           let decl = TypeDecl (pos, name, tp, VisLocal, false) in
           decl :: accum
       in
       convert decl_n_accum rest
    | CV_Variable (pos, name, ctype, extern) :: rest ->
       let faux_tokendata = [ { td_pos = pos;
                                td_text = name;
                                td_noncode = []
                              }
                            ]
       in
       let tp = get_lval (deftype_of ctype) in
       let decl = VarDecl (faux_global, faux_tokendata, [], tp,
                           if extern then VisExternal else VisLocal)
       in
       convert (decl :: accum) rest
  in
  let ret = convert [] cimport in
  let verify = function
    | TypeDecl (_, name, { bare = DefTypeUnresolved _ }, _, _) ->
       Report.err_internal __FILE__ __LINE__
                           ("Unresolved type " ^ name)
    | _ -> ()
  in
  List.iter verify ret;
  ret

let pos_of_cr = function
  | CRExpr pte -> pt_expr_pos pte
  | CRApproximate p -> p

let rec pos_of_astexpr expr = pos_of_cr expr.expr_cr

(** Return true if the expression is clearly always true in all
    circumstances. *)
let provably_always_true = function
  | { expr_ast = ExprLit (LitBool true) } -> true
  | _ -> false

(** Return true iff the expression is a spawn.  This can either be a simple
    function call, or a function call with a returned value. *)
let is_spawn_expr = function
  | { expr_ast = ExprFcnCall (_, _, true) }
  | { expr_ast =
        ExprBinary { op_right =
                       Some ({ expr_ast = ExprFcnCall (_, _, true) }) } } ->
     true
  | _ -> false
