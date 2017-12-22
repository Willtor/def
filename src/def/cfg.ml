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
open Parsetree
open Str
open Types
open Util

exception Arg2ParamMismatch

type cfg_expr =
  | Expr_New of Types.deftype * cfg_expr * (int * cfg_expr) list
  | Expr_FcnCall of string * cfg_expr list
  | Expr_FcnCall_Refs of string * (*args=*)string list
  | Expr_String of string * string (* label, contents *)
  | Expr_Binary of Ast.operator * bool * Types.deftype * cfg_expr * cfg_expr
  | Expr_Unary of Ast.operator * Types.deftype * cfg_expr * (*pre_p*)bool
  | Expr_Literal of Ast.literal
  | Expr_Variable of string
  | Expr_Cast of Types.deftype * Types.deftype * cfg_expr
  | Expr_Index of cfg_expr * cfg_expr * Types.deftype
                  * (*deref_base=*)bool * (*array=*)bool
  | Expr_SelectField of cfg_expr * int * (*is_volatile=*)bool
  | Expr_StaticStruct of string option * (Types.deftype * cfg_expr) list
  | Expr_StaticArray of cfg_expr list
  | Expr_Nil
  | Expr_Wildcard
  | Expr_Atomic of atomic_op * (Types.deftype * cfg_expr) list
  | Expr_Val_Ref of string

and atomic_op =
  | AtomicCAS
  | AtomicSwap

type cfg_basic_block =
  | BB_Seq of string * sequential_block
  | BB_Cond of string * conditional_block
  | BB_Term of string * terminal_block
  | BB_Detach of string * string * detach_block
  | BB_Reattach of string * string * sequential_block
  | BB_Sync of string * string * sequential_block
  | BB_Goto of string * sequential_block
  | BB_Error

and sequential_block =
  { mutable seq_prev  : cfg_basic_block list;
    mutable seq_next  : cfg_basic_block;
    mutable seq_expr  : (Lexing.position * cfg_expr) list;
    mutable seq_mark_bit : bool
  }

and conditional_block =
  { mutable cond_prev : cfg_basic_block list;
    mutable cond_next : cfg_basic_block;
    mutable cond_else : cfg_basic_block;
    cond_branch       : (Lexing.position * cfg_expr);
    mutable cond_mark_bit : bool;
    mutable cond_parallel : bool
  }

and terminal_block =
  { mutable term_prev : cfg_basic_block list;
    term_expr         : (Lexing.position * cfg_expr) option;
    mutable term_mark_bit : bool;
    term_xend         : bool
  }

and detach_block =
  { mutable detach_prev : cfg_basic_block list;
    detach_args : (string * cfg_expr) list;
    detach_ret  : (string * cfg_expr) option;
    mutable detach_next : cfg_basic_block;
    mutable detach_continuation : cfg_basic_block;
    mutable detach_mark_bit : bool
  }

and decl =
  { decl_pos   : Lexing.position;
    mappedname : string;
    vis        : Types.visibility;
    tp         : Types.deftype;
    params     : (Lexing.position * string) list (* Zero-length for non-fcns *)
  }

and function_defn =
  { defn_begin : Lexing.position;
    defn_end   : Lexing.position;
    name       : string;
    local_vars : (string * decl) list;
    mutable entry_bb : cfg_basic_block;
    fcn_cilk_init : string list
  }

type program =
  { global_decls : decl Util.symtab;
    initializers : (string, cfg_expr) Hashtbl.t;
    fcnlist : function_defn list;
    deftypemap : Types.deftype Util.symtab
  }

type function_scope =
  { fs_vars       : decl Util.symtab;
    fs_cont_bb    : cfg_basic_block option;
    fs_break_bb   : cfg_basic_block option;
    fs_sync_label : string;
    fs_in_xaction : bool;
  }

let make_fcn_scope vars =
  { fs_vars       = vars;
    fs_cont_bb    = None;
    fs_break_bb   = None;
    fs_sync_label = "bodysync";
    fs_in_xaction = false
  }

let combine3 a b c =
  let rec combine accum = function
    | [], [], [] -> List.rev accum
    | a1 :: arest, b1 :: brest, c1 :: crest ->
       combine ((a1, b1, c1) :: accum) (arest, brest, crest)
    | _ ->
       Report.err_internal __FILE__ __LINE__ "Inconsistent list lengths"
  in
  combine [] (a, b, c)

(* FIXME: Fill out the list of regular expressions. *)
let re_set = [(regexp "\\\\n", "\n")   (* newline *)
             ]

(* Visit a graph, depth-first. *)
let visit_df f bit =
  let rec visit param node =
    match node with
    | BB_Seq (_, block)
    | BB_Goto (_, block) ->
       if block.seq_mark_bit = bit then param
       else
         (block.seq_mark_bit <- bit;
          visit (f param node) block.seq_next)
    | BB_Cond (_, block) ->
       if block.cond_mark_bit = bit then param
       else
         (block.cond_mark_bit <- bit;
          let param = visit (f param node) block.cond_next in
          visit param block.cond_else)
    | BB_Term (_, block) ->
       if block.term_mark_bit = bit then param
       else
         (block.term_mark_bit <- bit;
          f param node)
    | BB_Detach (_, _, block) ->
       if block.detach_mark_bit = bit then param
       else
         (* WARNING: If this is ever used for something other than a normally
            structured spawn (where the continuation follows the spawned block
            then this will not cover the whole graph. *)
         (block.detach_mark_bit <- bit;
          visit (f param node) block.detach_next)
    | BB_Reattach (_, _, block) ->
       if block.seq_mark_bit = bit then param
       else
         (block.seq_mark_bit <- bit;
          visit (f param node) block.seq_next)
    | BB_Sync (_, _, block) ->
       if block.seq_mark_bit = bit then param
       else
         (block.seq_mark_bit <- bit;
          visit (f param node) block.seq_next)
    | BB_Error ->
       f param node
  in
  visit

(* Reset the marked bit throughout a CFG. *)
let reset_bbs = visit_df (fun _ _ -> ()) false ()

let label_of_pos pos =
  pos.pos_fname ^ "_" ^ (string_of_int pos.pos_lnum)
  ^ "_" ^ (string_of_int (pos.pos_cnum - pos.pos_bol))

(* FIXME: Unify with (not expr_must_be_true) in Scrubber. *)
let loop_can_exit = function
  | Expr_Literal (LitBool true) -> false
  | _ -> true

(* Evaluate the expression, if possible, and return a literal value. *)
let static_eval_expr = function
  | ExprLit (_, lit) -> Some lit
  | ExprBinary _ -> Report.err_internal __FILE__ __LINE__
     "Don't know nothin' 'bout no maths in array dimensions."
  | _ -> None

(* Detect if the loop can be exited (apart from a return from the actual
   function) from inside; i.e., see if there is a break or a goto. *)
let can_escape_forward labelmap stmts =
  let rec forward internal = function
    | [] -> internal, false
    | Break _ :: _ -> internal, true
    | Goto (_, label) :: rest ->
       (* If this label has already been found, it's earlier in the function
          and does not count as skipping forward. *)
       begin
         try let _ = Hashtbl.find labelmap label in
             forward internal rest
         with _ ->
           let check found bb =
             if found || (label = bb) then true
             else false
           in if List.fold_left check false internal then
                forward internal rest
              else internal, true
       end
    | IfStmt (_, _, tblock, eblock_maybe) :: rest ->
       begin
         let internal, result = forward internal tblock in
         if result then internal, true
         else match eblock_maybe with
              | None -> forward internal rest
              | Some eblock ->
                 let internal, result = forward internal eblock in
                 if result then internal, true
                 else forward internal rest
       end
    | Label (_, label) :: rest ->
       forward (label :: internal) rest
    | stmt :: rest -> forward internal rest
  in
  let _, result = forward [] stmts
  in result

(** Get the type of an Ast.literal value. *)
let typeof_literal lit =
  DefTypePrimitive (literal2primitive lit, [])

let rec get_field_types typemap = function
  | DefTypeNamedStruct nm ->
     get_field_types typemap (Util.the @@ lookup_symbol typemap nm)
  | DefTypeLiteralStruct (types, _) -> types
  | DefTypeStaticStruct types -> types
  | _ -> Report.err_internal __FILE__ __LINE__ "Unable to get field types."

let rec convert_type defining_p array2ptr typemap = function
  | VarType (pos, name, qlist) ->
     (* FIXME: qualifiers need to be addressed for all types. *)
     begin
       match lookup_symbol typemap name with
       | None ->
          (* If we're defining a type, this type may not yet have been
             declared.  If we aren't, then the type hasn't been declared
             at all and that's an error. *)
          if defining_p then DefTypeUnresolved (pos, name)
          else Report.err_unknown_typename pos name
       | Some (DefTypeLiteralStruct _) ->
          (* Prevent deep recursive definitions. *)
          DefTypeNamedStruct name
       | Some (DefTypePrimitive (p, _)) -> DefTypePrimitive (p, qlist)
       | Some t -> t
     end
  | FcnType (params, ret) ->
     let rec pconvert accum = function
       | [] -> false, List.rev accum
       | [ (_, _, Ellipsis _) ] -> true, List.rev accum
       | (_, _, Ellipsis pos) :: _ ->
          Report.err_vararg_not_last pos
       | (_, _, tp) :: rest ->
          pconvert (convert_type defining_p array2ptr typemap tp :: accum) rest
     in
     let variadic, converted_params = pconvert [] params in
     DefTypeFcn (converted_params,
                 convert_type defining_p array2ptr typemap ret,
                 variadic)
  | StructType elements ->
     let process (names, deftypes) (_, name, tp) =
       (name :: names,
        (convert_type defining_p array2ptr typemap tp) :: deftypes)
     in
     let names, deftypes = List.fold_left process ([], []) elements in
     DefTypeLiteralStruct (List.rev deftypes, List.rev names)
  | ArrayType (pos, dim_expr, tp) ->
     if array2ptr then
       DefTypePtr (convert_type defining_p array2ptr typemap tp, [])
     else
       let dim = match static_eval_expr dim_expr with
         | Some (LitBool b) -> if b then 1 else 0
         | Some (LitI8 c) | Some (LitU8 c) -> int_of_char c
         | Some (LitI16 n) | Some (LitU16 n)
         | Some (LitI32 n) | Some (LitU32 n) -> Int32.to_int n
         | Some (LitI64 n) | Some (LitU64 n) ->
            Int64.to_int n (* FIXME: Possible loss of
                            precision. *)
         | Some (LitF32 _) | Some (LitF64 _) ->
            Report.err_float_array_dim pos
         | None ->
            Report.err_cant_resolve_array_dim pos
       in
       DefTypeArray (convert_type defining_p array2ptr typemap tp, dim)
  | PtrType (pos, tp, qlist) ->
     DefTypePtr (convert_type defining_p array2ptr typemap tp, qlist)
  | Ellipsis pos ->
     Report.err_internal __FILE__ __LINE__
       ("Found an ellipsis at an unexpected location: "
        ^ (format_position pos))
  | InferredType ->
     Report.err_internal __FILE__ __LINE__
                         "Trying to convert an inferred type."

let param_pos_names = function
  | VarType _
  | ArrayType _
  | PtrType _
  | StructType _
  | Ellipsis _ -> []
  | FcnType (params, _) ->
     List.map (fun (pos, name, _) -> (pos, name)) params
  | InferredType ->
     Report.err_internal __FILE__ __LINE__
                         "Should not have inferred types in parameters."

let global_types typemap = function
  | TypeDecl (pos, name, tp, _, _) ->
     (* FIXME: When structs get added, check tp to see if it's a struct
        and add a symbolic reference, first, before converting the type. *)
     add_symbol typemap name (convert_type true false typemap tp)
  | _ -> ()

let resolve_type typemap typename oldtp =
  let rec v = function
    | DefTypeUnresolved (pos, name) ->
       begin match lookup_symbol typemap name with
       | Some (DefTypeLiteralStruct _) -> DefTypeNamedStruct name
       | Some tp -> v tp
       | None -> (* Unresolved type name. *)
          Report.err_unknown_typename pos name
       end
    | DefTypeNamedStruct _ as tp -> tp
    | DefTypeVoid as tp -> tp
    | DefTypePrimitive _ as tp -> tp
    | DefTypeFcn (params, ret, variadic) ->
       let params = List.map v params
       and ret = v ret
       in DefTypeFcn (params, ret, variadic)
    | DefTypePtr (tp, q) -> DefTypePtr (v tp, q)
    | DefTypeArray (tp, n) -> DefTypeArray (v tp, n)
    | DefTypeNullPtr -> DefTypeNullPtr
    | DefTypeLiteralStruct (fields, names) ->
       DefTypeLiteralStruct (List.map v fields, names)
    | DefTypeStaticStruct members ->
       DefTypeStaticStruct (List.map v members)
    | DefTypeWildcard -> DefTypeWildcard
    | DefTypeLLVMToken -> DefTypeLLVMToken
  in Some (v oldtp)

let rec infer_type_from_expr typemap scope = function
  | ExprNew (_, vt, _) ->
     DefTypePtr (convert_type false false typemap vt, [])
  | ExprFcnCall f ->
     let fdecl = Util.the (lookup_symbol scope f.fc_name) in
     begin match fdecl.tp with
     | DefTypeFcn (_, ret, _) -> ret
     | _ -> Report.err_internal __FILE__ __LINE__
                                "Function w/o function type."
     end
  | ExprString _ -> DefTypePtr (DefTypePrimitive (PrimI8, []), [])
  | ExprBinary op ->
     begin match op.op_op with
     | OperLogicalNot
     | OperLT | OperGT | OperLTE | OperGTE
     | OperEquals | OperNEquals
     | OperLogicalAnd | OperLogicalOr ->
        DefTypePrimitive (PrimBool, [])
     | OperAssign ->
        infer_type_from_expr typemap scope (Util.the op.op_right)
     | _ ->
        let lhs = infer_type_from_expr typemap scope op.op_left in
        let rhs = infer_type_from_expr typemap scope (Util.the op.op_right) in
        begin match lhs, rhs with
        | DefTypePrimitive (prim_left, _), DefTypePrimitive (prim_right, _) ->
           DefTypePrimitive (generalize_primitives prim_left prim_right, [])
        | a, b when a = b -> a
        | _ ->
           Report.err_internal __FILE__ __LINE__
                               "Couldn't generalize types."
        end
     end
  | ExprPreUnary op
  | ExprPostUnary op ->
     let tp = infer_type_from_expr typemap scope op.op_left in
     if op.op_op = OperAddrOf then DefTypePtr (tp, [])
     else tp
  | ExprVar (_, name) ->
     let decl = Util.the (lookup_symbol scope name) in
     decl.tp
  | ExprLit (_, literal) ->
     typeof_literal literal
  | ExprCast (_, vt, _) ->
     convert_type false false typemap vt
  | ExprIndex (_, base, _, _) ->
     begin match infer_type_from_expr typemap scope base with
     | DefTypePtr (tp, _)
     | DefTypeArray (tp, _) -> tp
     | _ ->
        Report.err_internal __FILE__ __LINE__ "Dereferenced unknown type."
     end
  | ExprSelectField (_, _, base, field) ->
     let rec get_field_tp = function
       | DefTypeNamedStruct name ->
          get_field_tp (Util.the (lookup_symbol typemap name))
       | DefTypeLiteralStruct (tlist, names) ->
          begin match field with
          | FieldNumber n -> List.nth tlist n
          | FieldName s ->
             let f (tp, nm) = if s = nm then true else false in
             let tp, _ = List.find f (List.combine tlist names) in
             tp
          end
       | DefTypeStaticStruct tlist ->
          begin match field with
          | FieldNumber n -> List.nth tlist n
          | _ -> Report.err_internal __FILE__ __LINE__
                                     "Field name on a static struct."
          end
       | _ -> Report.err_internal __FILE__ __LINE__
                                  "Trying to get field from non-struct."
     in
     get_field_tp (infer_type_from_expr typemap scope base)
  | ExprStaticStruct (_, fields) ->
     let ftypes =
       List.map
         (fun (_, e) -> infer_type_from_expr typemap scope e) fields
     in
     (* Becomes a literal struct since variables are never static structs. *)
     DefTypeLiteralStruct (ftypes, [])
  | ExprStaticArray (pos, elements) ->
     let atypes =
       List.map
         (fun (_, e) -> infer_type_from_expr typemap scope e) elements
     in
     let tp = most_general_type pos typemap atypes in
     DefTypeArray (tp, List.length elements)
  | ExprType _ ->
     Report.err_internal __FILE__ __LINE__ "Type of type?"
  | ExprTypeString _ -> DefTypePtr (DefTypePrimitive (PrimI8, []), [])
  | ExprNil _ ->
     Report.err_internal __FILE__ __LINE__ "Type of nil?"
  | ExprWildcard _ ->
     Report.err_internal __FILE__ __LINE__ "Type of wildcard?"

let global_decls decltable typemap = function
  | DeclFcn (pos, vis, name, tp)
  | DefFcn (pos, _, vis, name, tp, _) ->
     begin match lookup_symbol decltable name with
     | Some _ -> ()
     | None ->
        let fcn =
          { decl_pos = pos;
            mappedname = name;
            vis = vis;
            tp = convert_type false false typemap tp;
            params = param_pos_names tp
          }
        in
        add_symbol decltable name fcn
     end
  | VarDecl (vars, inits, tp) ->
     let f (tok, init) = match lookup_symbol decltable tok.td_text with
       | Some decl ->
          Report.err_redefined_var tok.td_pos decl.decl_pos tok.td_text
       | None ->
          let convtp =
            if tp = InferredType then
              if init = None then
                Report.err_no_init_on_inferred_type tok.td_pos
              else infer_type_from_expr typemap decltable (Util.the init)
            else convert_type false false typemap tp
          in
          let decl =
            { decl_pos = tok.td_pos;
              mappedname = tok.td_text;
              vis = VisLocal; (* FIXME *)
              tp = convtp;
              params = []
            }
          in
          add_symbol decltable tok.td_text decl
     in
     let add_none accum _ = None :: accum in
     let add_some init = Some init in
     let ilist = if inits <> [] then List.map add_some inits
                 else List.fold_left add_none [] vars
     in
     List.iter f (List.combine vars ilist)
  | Import _ -> ()
  | TypeDecl _ -> ()
  | DefTemplateFcn _ ->
     Report.err_internal __FILE__ __LINE__
                         "Templates should have been resolved."
  | _ -> Report.err_internal __FILE__ __LINE__
     "FIXME: Incomplete implementation of Cfg.global_decls."

let get_fcntype_profile = function
  | DefTypeFcn (params, ret, variadic) -> params, ret, variadic
  | _ -> Report.err_internal __FILE__ __LINE__ " Unexpected function type."

let rec make_size_expr typemap p = function
  | ArrayType (ap, e, tp) ->
     let subsize = make_size_expr typemap ap tp in
     let op = { op_pos = ap;
                op_op = OperMult;
                op_left = e;
                op_right = Some subsize;
                op_atomic = false
              }
     in
     ExprBinary op
  | tp ->
     let sz = size_of typemap (convert_type false false typemap tp) in
     ExprLit (p, LitU32 (Int32.of_int sz))

(** Return a casted version of the expression, if the original type doesn't
    match the desired type. *)
let rec maybe_cast typemap orig cast_as expr =
  if orig = cast_as || orig = DefTypeWildcard then expr
  else match cast_as with
  | DefTypeNamedStruct nm ->
     begin match the (lookup_symbol typemap nm) with
     | DefTypeLiteralStruct (tplist, _) ->
        begin match expr with
        | Expr_StaticStruct (_, exprmembers) ->
           let castmembers =
             List.map2
               (fun to_tp (from_tp, e) ->
                 to_tp, (maybe_cast typemap from_tp to_tp e))
               tplist
               exprmembers
           in
           Expr_StaticStruct (Some nm, castmembers)
        | _ ->
           Report.err_internal __FILE__ __LINE__ "Non-struct type struct."
        end
     | _ -> Report.err_internal __FILE__ __LINE__ "Non-struct type struct."
     end
  | DefTypePrimitive (p1, p1q) ->
     begin match orig with
     | DefTypePrimitive (p2, p2q) ->
        (* FIXME: Removing qualifiers should generate warnings/errors. *)
        if p1 = p2 then expr
        else Expr_Cast (orig, cast_as, expr)
     | _ -> Expr_Cast (orig, cast_as, expr)
     end
  | _ ->
     if expr = Expr_Nil then
       let zero = Expr_Literal (LitU64 (Int64.of_int 0)) in
       let tp = DefTypePrimitive (PrimU64, []) in
       if tp = cast_as then zero
       else Expr_Cast (tp, cast_as, zero)
     else Expr_Cast (orig, cast_as, expr)

(** Determine whether one type can be cast as another.  This function returns
    unit, as it only reports an error if it fails. *)
let check_castability pos typemap ltype rtype =
  let rec similar = function
    | DefTypePrimitive _, DefTypePrimitive _ ->
       ()  (* FIXME: Implement. *)
    | DefTypeFcn (plist1, ret1, v1), DefTypeFcn (plist2, ret2, v2) ->
       begin
         List.iter identical (List.combine plist1 plist2);
         identical (ret1, ret2);
         if v1 != v2 then Report.err_type_mismatch pos
       end
    | DefTypePtr (DefTypeVoid, _), DefTypePtr _
    | DefTypePtr _, DefTypePtr (DefTypeVoid, _) ->
       () (* FIXME: qualifier comparison? *)
    | DefTypePtr (p1, _), DefTypePtr (p2, _) ->
       identical (p1, p2) (* FIXME: qualifier comparison? *)
    | DefTypeStaticStruct smembers, DefTypeNamedStruct nm
    | DefTypeNamedStruct nm, DefTypeStaticStruct smembers ->
       begin match lookup_symbol typemap nm with
       | Some DefTypeLiteralStruct (lmembers, _) ->
          List.iter similar (List.combine lmembers smembers)
       | _ -> Report.err_internal __FILE__ __LINE__ "Unknown struct type."
       end
    | DefTypeStaticStruct smembers, DefTypeLiteralStruct (lmembers, _)
    | DefTypeLiteralStruct (lmembers, _), DefTypeStaticStruct smembers ->
       List.iter similar (List.combine lmembers smembers)
    | DefTypeLiteralStruct (lhs, _), DefTypeLiteralStruct (rhs, _) ->
       List.iter similar (List.combine lhs rhs)
    | DefTypePrimitive _, _ ->
       Report.err_internal __FILE__ __LINE__ (format_position pos)
    | DefTypePtr _, DefTypeNullPtr
    | DefTypeNullPtr, DefTypePtr _ -> () (* FIXME: Qualifier comparison? *)
    | DefTypeArray _, DefTypePtr _ -> () (* FIXME: Qualifier comparison? *)
    | DefTypeArray (t1, n), DefTypeArray (t2, m) when m = n && t1 = t2 ->
       ()
    | (DefTypeArray _ as t1), (DefTypeArray _ as t2) ->
       Report.err_cant_cast pos (string_of_type t1) (string_of_type t2)
    | t1, t2 ->
       Report.err_internal __FILE__ __LINE__
                           ("incomplete cast for " ^ (string_of_type t1)
                            ^ " to " ^ (string_of_type t2) ^ ": "
                            ^ (format_position pos))
  and identical (ltype, rtype) =
    if ltype = rtype then ()
    else
      begin
        prerr_endline (string_of_type ltype);
        prerr_endline (string_of_type rtype);
        Report.err_type_mismatch pos
      end
  in
  similar (ltype, rtype)

(** Reconcile the types of two subexpressions connected by a binary operator
    and return the result.  The result may include implicit casts. *)
let binary_reconcile typemap =
  let reconcile pos op (ltype, lexpr) (rtype, rexpr) =
    match op with
    | OperPlus | OperMinus | OperMult | OperDiv
    | OperLShift | OperRShift ->
       let tp = most_general_type pos typemap [ltype; rtype] in
       tp, tp,
       (maybe_cast typemap ltype tp lexpr),
       (maybe_cast typemap rtype tp rexpr)
    | OperRemainder ->
       if (not (is_integer_type ltype)) || (not (is_integer_type rtype)) then
         Report.err_modulo_on_non_integer
           pos (string_of_type ltype) (string_of_type rtype)
       else
         let tp = most_general_type pos typemap [ltype; rtype] in
         tp, tp,
         (maybe_cast typemap ltype tp lexpr),
         (maybe_cast typemap rtype tp rexpr)
    | OperLT | OperLTE
    | OperGT | OperGTE
    | OperEquals | OperNEquals ->
       let tp = most_general_type pos typemap [ltype; rtype] in
       DefTypePrimitive (PrimBool, []),
       tp,
       (maybe_cast typemap ltype tp lexpr),
       (maybe_cast typemap rtype tp rexpr)
    | OperBitwiseAnd | OperBitwiseOr ->
       let tp = most_general_type pos typemap [ltype; rtype] in
       tp,
       tp,
       maybe_cast typemap ltype tp lexpr,
       maybe_cast typemap rtype tp rexpr
    | OperLogicalOr | OperLogicalAnd ->
       let primbool = DefTypePrimitive (PrimBool, []) in
       begin
         check_castability pos typemap ltype primbool;
         check_castability pos typemap rtype primbool;
         primbool,
         primbool,
         maybe_cast typemap ltype primbool lexpr,
         maybe_cast typemap rtype primbool rexpr
       end
    | OperAssign
    | OperPlusAssign | OperMinusAssign ->
       begin
         (* FIXME: Do automatic casting of static structs to named or literal
            structs, if possible.  Otherwise, casting doesn't work.  Literal
            structs need to be the correct type at instantiation. *)
         check_castability pos typemap ltype rtype;
         ltype, ltype, lexpr, (maybe_cast typemap rtype ltype rexpr)
       end
    | _ -> Report.err_internal __FILE__ __LINE__
       ("FIXME: Incomplete implementation Cfg.reconcile (operator "
        ^ (operator2string op) ^ ").")
  in reconcile

(** Convert a function call, verifying the function profile and matching
    arguments with parameters. *)
let build_fcn_call scope typemap pos name args =
  match lookup_symbol scope name with
  | None ->
     let ptr2int tp = match tp with
       | DefTypePtr _ -> DefTypePrimitive (PrimU64, [])
       | _ -> tp
     in
     let create_atomicrmw op = match args with
       | [(DefTypePtr (t1, q), dexpr); (t2, vexpr)] ->
          (* FIXME: Do more checking on the type of the destination arg. *)
          let castptrs = ptr2int t1 in
          let ptr2t1 = DefTypePtr (t1, q) in
          let ptr2castptrs = DefTypePtr (castptrs, []) in
          let revised = [ (DefTypePtr (castptrs, []),
                           maybe_cast typemap ptr2t1 ptr2castptrs dexpr);
                          (castptrs, maybe_cast typemap t2 castptrs vexpr) ]
          in
          let fcn = Expr_Atomic (op, revised) in
          t1, maybe_cast typemap castptrs t1 fcn
       | [_; _] ->
          Report.err_atomic_dest_not_ptr pos name
       | _ ->
          Report.err_wrong_number_of_atomic_args pos name (List.length args)
     in
     let create_atomic_cas () = match args with
       | [(DefTypePtr (t1, q), dexpr); (t2, cmpexpr); (t3, valexpr)] ->
          let rettp = DefTypePrimitive (PrimBool, []) in
          let castptrs = ptr2int t1 in
          let ptr2t1 = DefTypePtr (t1, q) in
          let ptr2castptrs = DefTypePtr (castptrs, []) in
          let revised = [ (DefTypePtr (castptrs, []),
                           maybe_cast typemap ptr2t1 ptr2castptrs dexpr);
                          (castptrs, maybe_cast typemap t2 castptrs cmpexpr);
                          (castptrs, maybe_cast typemap t3 castptrs valexpr) ]
          in
          rettp, Expr_Atomic (AtomicCAS, revised)
       | _ ->
          Report.err_internal __FILE__ __LINE__ "Need an error message for CAS"
     in
     (* Check if the function is an atomic. *)
     begin match name with
     | "__builtin_cas" ->
        if (List.length args) <> 3 then
          Report.err_wrong_number_of_atomic_args pos name (List.length args)
        else
          create_atomic_cas ()
     | "__builtin_swap" ->
        create_atomicrmw AtomicSwap
     | _ -> Report.err_unknown_fcn_call pos name
     end
  | Some decl ->
     begin match decl.tp with
     | DefTypeUnresolved _ -> Report.err_internal __FILE__ __LINE__
        "Tried to call an unresolved type."
     | DefTypeNamedStruct _ -> Report.err_internal __FILE__ __LINE__
        "Tried to call a named-struct-type."
     | DefTypeVoid -> Report.err_internal __FILE__ __LINE__
        "Tried to call a void."
     | DefTypeFcn (params, rettp, variadic) ->
        let rec match_params_with_args accum = function
          | [], [] -> List.rev accum
          | [], (_, expr) :: args ->
             if variadic then
               match_params_with_args (expr :: accum) ([], args)
             else
               raise Arg2ParamMismatch
          | params, [] -> raise Arg2ParamMismatch
          | ptype :: params, (atype, expr) :: args ->
             let () = check_castability pos typemap atype ptype in
             let cast_arg = maybe_cast typemap atype ptype expr in
             match_params_with_args (cast_arg :: accum) (params, args)
        in
        begin
          try
            let cast_args = match_params_with_args [] (params, args) in
            rettp, Expr_FcnCall (decl.mappedname, cast_args)
          with _ ->
            Report.err_wrong_number_of_args pos decl.decl_pos name
              (List.length params) (List.length args)
        end
     | DefTypePrimitive _ -> Report.err_called_non_fcn pos decl.decl_pos name
     | DefTypePtr _ -> Report.err_internal __FILE__ __LINE__
        "Tried to call a pointer."
     | DefTypeArray _ -> Report.err_internal __FILE__ __LINE__
        "Tried to call an array."
     | DefTypeNullPtr -> Report.err_internal __FILE__ __LINE__
        "Tried to call nil."
     | DefTypeLiteralStruct _
     | DefTypeStaticStruct _ -> Report.err_internal __FILE__ __LINE__
        "Tried to call a struct."
     | DefTypeWildcard ->
        Report.err_internal __FILE__ __LINE__ "Tried to call a wildcard."
     | DefTypeLLVMToken ->
        Report.err_internal __FILE__ __LINE__
                            "Tried to call an LLVM Token."
     end

(** Convert an expression, verifying that all operations are valid on the
    types, and things like variables are valid in the current scope. *)
let convert_expr typemap fcnscope =
  let scope = fcnscope.fs_vars in
  let rec convert = function
    | ExprNew (pos, t, init) ->
       let tp = match t with
         (* Throw away an initial ArrayType, since that's only relevant for
            the size of the allocation. *)
         | ArrayType (_, _, vt) -> convert_type false false typemap vt
         | _ -> convert_type false false typemap t
       in
       (* FIXME: Fix size for variable-sized array members. *)
       let i32tp, i32sz = convert (make_size_expr typemap pos t) in
       let i64sz =
         maybe_cast typemap i32tp (DefTypePrimitive (PrimI64, [])) i32sz in
       begin match init with
       | [] -> DefTypePtr (tp, []), Expr_New (tp, i64sz, [])
       | _ ->
          let mtypes, fields = match tp with
            | DefTypeNamedStruct sname ->
               begin match the (lookup_symbol typemap sname) with
               | DefTypeLiteralStruct (mtypes, fields) -> mtypes, fields
               | _ ->
                  Report.err_internal __FILE__ __LINE__
                                      ("Struct " ^ sname ^ " is not literal")
               end
            | _ -> Report.err_internal __FILE__ __LINE__
                                       "Used new in an unsupported context."
          in
          let lookup_field fp f =
            let rec lookup n = function
              | [], _ | _, []-> Report.err_struct_no_such_member fp f
              | tp :: trest, fieldname :: frest ->
                 if f = fieldname then tp, n
                 else lookup (n + 1) (trest, frest)
            in
            lookup 0 (mtypes, fields)
          in
          let init_convert = function
            | (fp, f, None, ep, e) ->
               let mtype, n = lookup_field fp f in
               let conv_tp, conv_e = convert e in
               let () = check_castability ep typemap conv_tp mtype in
               n, maybe_cast typemap conv_tp mtype conv_e
            | (fp, f, Some (ap, ae), ep, e) ->
               Report.err_internal __FILE__ __LINE__ "Not implemented, yet."
          in
          let field_inits = List.map init_convert init in
          DefTypePtr (tp, []), Expr_New (tp, i64sz, field_inits)
       end
    | ExprFcnCall call ->
       if call.fc_spawn then
         Report.err_bad_spawn_loc call.fc_pos
       else
         let converted_args = List.map convert call.fc_args in
         let nm = Templates.mangle call.fc_name call.fc_template in
         build_fcn_call scope typemap call.fc_pos nm converted_args
    | ExprString (pos, str) ->
       let raw =
         List.fold_left
           (fun s (re, subst) -> global_replace re subst s)
           str
           re_set
       in
       (* FIXME: Should be const. *)
       let tp =
         DefTypeArray (DefTypePrimitive (PrimI8, []), String.length raw) in
       tp, Expr_String (label_of_pos pos, raw)
    | ExprBinary op ->
       let rettp, tp, lhs, rhs =
         binary_reconcile typemap op.op_pos op.op_op
           (convert op.op_left) (convert (the op.op_right))
       in
       (* FIXME: Should we make non-integers work using transactions?  We
          _could_.  Do users want that? *)
       if op.op_atomic && not ((is_integer_type tp) || (is_pointer_type tp))
       then Report.err_atomic_non_integer op.op_pos (string_of_type tp)
       else rettp, Expr_Binary (op.op_op, op.op_atomic, tp, lhs, rhs)
    | ExprPreUnary op ->
       let tp, subexpr = convert op.op_left in
       let rettp = match op.op_op with
         | OperAddrOf -> DefTypePtr (tp, [])
         | _ -> tp
       in
       rettp, Expr_Unary (op.op_op, tp, subexpr, true)
    | ExprVar (pos, name) ->
       begin match lookup_symbol scope name with
       | Some var ->
          var.tp, Expr_Variable var.mappedname (* FIXME! Wrong type. *)
       | None ->
          Report.err_undefined_var pos name
       end
    | ExprLit (pos, literal) ->
       (typeof_literal literal), Expr_Literal literal
    | ExprIndex (bpos, base, ipos, idx) ->
       let btype, converted_base = convert base
       and itype, converted_idx = convert idx
       in begin match btype with
       | DefTypePtr (DefTypeVoid, _) -> Report.err_deref_void_ptr bpos ipos
       | DefTypeArray (deref_type, _) ->
          if is_integer_type itype then
            deref_type,
            Expr_Index (converted_base, converted_idx, deref_type, false, true)
          else
            Report.err_non_integer_index ipos
       | DefTypePtr (deref_type, _) ->
          if is_integer_type itype then
            deref_type,
            Expr_Index (converted_base, converted_idx, deref_type, true, false)
          else
            Report.err_non_integer_index ipos
       | _ -> Report.err_index_non_ptr ipos
       end
    | ExprSelectField (dpos, fpos, obj, field) ->
       let otype, converted_obj = convert obj in
       let rec struct_select obj = function
         | DefTypeLiteralStruct (mtypes, fields) ->
            let id = match field with
            | FieldNumber n ->
               if n >= (List.length mtypes) then
                 Report.err_struct_not_enough_fields fpos n
               else n
            | FieldName fieldname ->
               let rec get_field n = function
                 | [] -> Report.err_struct_no_such_member fpos fieldname
                 | f :: rest ->
                    if f = fieldname then n else get_field (n + 1) rest
               in
               get_field 0 fields
            in
            let is_volatile = function
              | DefTypePrimitive (_, [ Volatile ]) -> true
              | _ -> false
            in
            let tp = List.nth mtypes id in
            tp,
            Expr_SelectField (obj, id, is_volatile tp)
         | DefTypeNamedStruct sname ->
            struct_select obj (the (lookup_symbol typemap sname))
         | DefTypePtr (p, _) ->
            let idx = LitI32 (Int32.of_int 0) in
            let derefed_obj =
              Expr_Index (obj, Expr_Literal idx, p, true, false) in
            struct_select derefed_obj p
         | _ -> Report.err_non_struct_member_access dpos
       in
       struct_select converted_obj otype
    | ExprCast (pos, to_tp, e) ->
       let cast_tp = convert_type false true typemap to_tp in
       let orig_tp, converted_expr = convert e in
       cast_tp, maybe_cast typemap orig_tp cast_tp converted_expr
    | ExprStaticStruct (_, members) ->
       let cmembers = List.map (fun (_, e) -> convert e) members in
       let tlist = List.rev (List.fold_left (fun taccum (t, _) ->
                                 (t :: taccum)) [] cmembers) in
       DefTypeStaticStruct tlist, Expr_StaticStruct (None, cmembers)
    | ExprStaticArray (pos, elements) ->
       let ce = List.map (fun (_, e) -> convert e) elements in
       let tlist = List.rev (List.fold_left (fun taccum (t, _) ->
                                 (t :: taccum)) [] ce) in
       let tp = most_general_type pos typemap tlist in
       let cast_ce = List.map (fun (t, e) -> maybe_cast typemap t tp e) ce in
       DefTypeArray (tp, List.length tlist), Expr_StaticArray cast_ce
    | ExprType (pos, _) -> Report.err_unexpected_type_expr pos
    | ExprTypeString (pos, subexpr) ->
       let nm = (label_of_pos pos) ^ ".type_str" in
       let expr_tp = infer_type_from_expr typemap scope subexpr in
       DefTypePtr (DefTypePrimitive (PrimI8, []), []),
       Expr_String (nm, string_of_type expr_tp)
    | ExprNil _ -> DefTypeNullPtr, Expr_Nil
    | ExprWildcard _ -> DefTypeWildcard, Expr_Wildcard
    | e ->
       Report.err_internal __FILE__ __LINE__
                           ("Cfg.convert_expr not fully implemented.\n"
                            ^ (format_position (pos_of_astexpr e)))
  in convert

let nonconflicting_name pos scope name =
  match lookup_symbol_local scope name with
  | None -> begin match lookup_symbol scope name with
    | None -> name
    | _ ->
       "_def_" ^ name ^ "_" ^ (string_of_int pos.pos_lnum)
  end
  | Some decl -> Report.err_redeclared_variable pos decl.decl_pos name

let make_decl pos fcnscope nm tp =
  let vars = fcnscope.fs_vars in
  { decl_pos = pos;
    mappedname = nonconflicting_name pos vars nm;
    vis = VisLocal;
    tp = tp;
    params = []
  }

let make_sequential_bb label exprs =
  BB_Seq (label, { seq_prev = [];
                   seq_next = BB_Error;
                   seq_expr = exprs;
                   seq_mark_bit = false
                 })

let make_conditional_bb label cond =
  BB_Cond (label, { cond_prev = [];
                    cond_next = BB_Error;
                    cond_else = BB_Error;
                    cond_branch = cond;
                    cond_mark_bit = false;
                    cond_parallel = false
                  })

let set_cond_parallel = function
  | BB_Cond (_, cond) ->
     cond.cond_parallel <- true
  | _ -> Report.err_internal __FILE__ __LINE__
                             "Tried to parallelize a non BB_Cond"

let make_terminal_bb label xend rexpr =
  BB_Term (label, { term_prev = [];
                    term_expr = rexpr;
                    term_mark_bit = false;
                    term_xend = xend
                  })

let make_detach_bb label sync args ret =
  let rename base (_, e) = (base ^ "." ^ (Util.unique_id ())), e in
  BB_Detach (label,
             sync, { detach_prev = [];
                     detach_args = List.map (rename "arg") args;
                     detach_ret = (match ret with
                                   | None -> None
                                   | Some ret -> Some (rename "ret" ret));
                     detach_next = BB_Error;
                     detach_continuation = BB_Error;
                     detach_mark_bit = false
                   })

let make_reattach_bb label sync exprs =
  BB_Reattach (label,
               sync, { seq_prev = [];
                       seq_next = BB_Error;
                       seq_expr = exprs;
                       seq_mark_bit = false
                     })

let make_sync_bb label sync =
  BB_Sync (label,
           sync, { seq_prev = [];
                   seq_next = BB_Error;
                   seq_expr = [];
                   seq_mark_bit = false
                 })

let make_goto_bb label =
  BB_Goto (label, { seq_prev = [];
                    seq_next = BB_Error;
                    seq_expr = [];
                    seq_mark_bit = false
                  })

let rec build_bbs name decltable typemap body =
  let fcndecl = the (lookup_symbol decltable name) in
  let param_types, ret_type, _ = get_fcntype_profile fcndecl.tp in

  (* Add the function's parameters to the scope table. *)
  let scoped_vars = push_symtab_scope decltable in
  List.iter2 (fun tp (pos, name) ->
    add_symbol scoped_vars name
      { decl_pos = pos;
        mappedname = name;
        vis = VisLocal;
        tp = tp;
        params = [] })
    param_types
    fcndecl.params;

  (* Create a two-way link between bb1 -> bb2. *)
  let add_next bb1 bb2 =
    let push_next bb = function
      | BB_Seq (_, block)
      | BB_Reattach (_, _, block)
      | BB_Sync (_, _, block) ->
         if block.seq_next = BB_Error then
           block.seq_next <- bb
      | BB_Detach (_, _, block) ->
         if block.detach_next = BB_Error then block.detach_next <- bb
         else if block.detach_continuation = BB_Error then
           block.detach_continuation <- bb
      | BB_Cond (_, block) ->
         if block.cond_next = BB_Error then block.cond_next <- bb
         else if block.cond_else = BB_Error then block.cond_else <- bb
      | BB_Goto _ (* Goto doesn't get a "next." *)
      | BB_Term _
      | BB_Error -> ()
    in
    let push_prev bb = function
      (* FIXME: Shouldn't add if push_next didn't make the change. *)
      | BB_Seq (_, block)
      | BB_Reattach (_, _, block)
      | BB_Sync (_, _, block)
      | BB_Goto (_, block) ->
         block.seq_prev <- bb :: block.seq_prev
      | BB_Detach (_, _, block) ->
         block.detach_prev <- bb :: block.detach_prev
      | BB_Cond (_, block) ->
         block.cond_prev <- bb :: block.cond_prev
      | BB_Term (_, block) ->
         block.term_prev <- bb :: block.term_prev
      | BB_Error -> ()
    in
    push_next bb2 bb1;
    push_prev bb1 bb2
  in

  (* Make a conditional CFG that does short-circuiting for logical operators.
  *)
  let rec generate_conditional str pos expr succ fail =
    match expr with
    | Expr_Binary (OperLogicalAnd, _, _, left, right) ->
       let right_bb = generate_conditional (str ^ "r") pos right succ fail in
       generate_conditional (str ^ "l") pos left right_bb fail
    | Expr_Binary (OperLogicalOr, _, _, left, right) ->
       let right_bb = generate_conditional (str ^ "r") pos right succ fail in
       generate_conditional (str ^ "l") pos left succ right_bb
    | e ->
       let cond_bb = make_conditional_bb str (pos, e) in
       let () = add_next cond_bb succ in
       let () = add_next cond_bb fail in
       cond_bb
  in

  let label_bbs = Hashtbl.create 8 in

  let sync_regions = ref [] in
  let push_sync_region region = sync_regions := region :: !sync_regions in
  let unique_regions () =
    let ordered_regions = List.sort String.compare !sync_regions in
    let uniquify accum str = match accum with
      | [] -> [ str ]
      | last :: _ -> if str = last then accum
                     else str :: accum
    in
    List.fold_left uniquify [] ordered_regions
  in

  let decls = ref [] in
  let add_decl name decl = decls := (name, decl) :: !decls in

  let push_scope ?(cont = None)
                 ?(break = None)
                 ?(sync = None)
                 ?(xaction = false)
                 fs =
    { fs_vars = push_symtab_scope fs.fs_vars;
      fs_cont_bb = if cont <> None then cont else fs.fs_cont_bb;
      fs_break_bb = if break <> None then break else fs.fs_break_bb;
      fs_sync_label = if sync <> None then Util.the sync else fs.fs_sync_label;
      fs_in_xaction = if xaction then true else fs.fs_in_xaction
    }
  in

  (* Iterate through the basic blocks and convert them to the CFG structure
     along with all of the contained expressions.  This function will leave
     temporary placeholders, like BB_Goto and BB_Error, so the CFG needs
     cleanup. *)
  let rec process_bb scope prev_bb = function
    | [] -> prev_bb
    | StmtExpr (pos, expr) :: rest ->
       let label = label_of_pos pos in
       let sync_label = scope.fs_sync_label in
       let make_spawn nm lhs args =
         let () = push_sync_region sync_label in
         (* FIXME: Check order of things.  Is this what we want to do?
            1. Compute arguments.
            2. Compute left-hand-side (lhs).
            3. Perform detach.
            4. Call function and assign return value to lhs.
            5. Continuation.
            Do 1 and 2 want to be swapped?
          *)
         let args = List.map (convert_expr typemap scope) args in
         let ret = match lhs with
           | None -> None
           | Some lhs ->
              let lvalue = ExprPreUnary { op_pos = faux_pos;
                                          op_op = OperAddrOf;
                                          op_left = lhs;
                                          op_right = None;
                                          op_atomic = false;
                                        }
              in
              Some (convert_expr typemap scope lvalue)
         in
         let detach = make_detach_bb ("detach." ^ label) sync_label args ret in
         let block = match detach with
           | BB_Detach (_, _, block) -> block
           | _ -> Report.err_internal __FILE__ __LINE__ "Do you even lift?"
         in
         let call = Expr_FcnCall_Refs (nm, List.map (fun (label, _) -> label)
                                                    block.detach_args)
         in detach, call
       in
       let begin_bb, end_bb = match expr with
         | ExprFcnCall ({ fc_spawn = true } as fc) ->
            let detach, call = make_spawn fc.fc_name None fc.fc_args in
            let reattach =
              make_reattach_bb (label ^ ".reattach") sync_label
                               [(fc.fc_pos, call)] in
            let end_bb = make_sequential_bb (label ^ ".cont") [] in
            let () = add_next detach reattach in
            let () = add_next reattach end_bb in
            let () = add_next detach end_bb in
            detach, end_bb
         | ExprBinary ({ op_op = OperAssign;
                         op_left = lhs;
                         op_right =
                           Some (ExprFcnCall ({ fc_spawn = true } as fc))
                      }) ->
            (* FIXME: Do casting of return value, if necessary. *)
            let detach, call = make_spawn fc.fc_name (Some lhs) fc.fc_args in
            let lhslabel = match detach with
              | BB_Detach (_, _, { detach_ret = Some (retlabel, _) }) ->
                 retlabel
              | _ -> Report.err_internal __FILE__ __LINE__ "ruh roh!"
            in
            let decl = Util.the (lookup_symbol scope.fs_vars fc.fc_name) in
            let tp = match decl.tp with
              | DefTypeFcn (_, rettp, _) -> rettp
              | _ -> Report.err_internal __FILE__ __LINE__ "Bad function type."
            in
            let expr =
              Expr_Binary (OperAssign, false, tp, Expr_Val_Ref lhslabel, call)
            in
            let reattach =
              make_reattach_bb (label ^ ".reattach") sync_label
                               [(fc.fc_pos, expr)] in
            let end_bb = make_sequential_bb (label ^ ".cont") [] in
            let () = add_next detach reattach in
            let () = add_next reattach end_bb in
            let () = add_next detach end_bb in
            detach, end_bb
         | _ ->
            let _, expr = convert_expr typemap scope expr in
            let bb = make_sequential_bb ("expr_" ^ label) [(pos, expr)] in
            bb, bb
       in
       let () = add_next prev_bb begin_bb in
       process_bb scope end_bb rest
    | Block (_, stmts) :: rest ->
       let bb =
         process_bb (push_scope scope) prev_bb stmts in
       process_bb scope bb rest
    | DeclFcn _ :: _ ->
       Report.err_internal __FILE__ __LINE__ "DeclFcn not expected."
    | DefFcn (pos, _, _, _, _, _) :: _ ->
       Report.err_internal __FILE__ __LINE__
                           ("At " ^ (format_position pos)
                            ^ ": local functions not yet implemented.")
    | DefTemplateFcn _ :: _ ->
       Report.err_internal __FILE__ __LINE__ "DefTemplateFcn not supported."
    | VarDecl (vars, inits, tp) :: rest ->
       let t = match tp with
         | InferredType ->
            (* Get the type from the rhs. *)
            let tp_list =
              List.map (infer_type_from_expr typemap scope.fs_vars) inits
            in
            (* FIXME: Should do error checking on whether the types of all
               right-hand sides match each other. *)
            List.hd tp_list
         | _ -> convert_type false false typemap tp
       in
       let declare var =
         let decl = make_decl var.td_pos scope var.td_text t in
         add_symbol scope.fs_vars var.td_text decl;
         add_decl var.td_text decl
       in
       let initialize bb expr =
         let faux_stmt = StmtExpr (pos_of_astexpr expr, expr) in
         process_bb scope bb [faux_stmt]
       in
       let () = List.iter declare vars in
       let bb = List.fold_left initialize prev_bb inits in
       process_bb scope bb rest
    | InlineStructVarDecl (p, vars, (epos, expr)) :: rest ->
       let temp_struct_nm = "inline_struct." ^ (unique_id ()) in
       let target_tp = infer_type_from_expr typemap scope.fs_vars expr in
       let target_struct = make_decl p scope temp_struct_nm target_tp
       in
       let () = add_symbol scope.fs_vars temp_struct_nm target_struct in
       let () = add_decl temp_struct_nm target_struct in
       let _, cexpr = convert_expr typemap scope
         (ExprBinary { op_pos = epos;
                       op_op = OperAssign;
                       op_left = ExprVar (p, temp_struct_nm);
                       op_right = Some expr;
                       op_atomic = false
                     })
       in
       let field_types = get_field_types typemap target_tp in
       let var_init n (pos, nm, tp) =
         let field_type = List.nth field_types n in
         let vartp = match tp with
           | InferredType -> field_type
           | _ -> convert_type false false typemap tp
         in
         let decl = make_decl pos scope nm vartp in
         let () = add_symbol scope.fs_vars nm decl in
         let () = add_decl nm decl in
         let rhs = ExprSelectField (pos, pos,
                                    ExprVar (pos, temp_struct_nm),
                                    FieldNumber n) in
         let _, cexpr =
           convert_expr typemap scope
                        (ExprBinary { op_pos = pos;
                                      op_op = OperAssign;
                                      op_left = ExprVar (pos, nm);
                                      op_right = Some rhs;
                                      op_atomic = false
                        })
         in
         pos, cexpr
       in
       let inits = List.mapi var_init vars in
       let bb =
         make_sequential_bb (label_of_pos p) ((p, cexpr) :: inits) in
       let () = add_next prev_bb bb in
       process_bb scope bb rest
    | TransactionBlock (pos, body) :: rest ->
       (* FIXME: Breaking/continuing/goto-ing out of the block should commit
                 the transaction. *)
       let pos_label = label_of_pos pos in
       let _, f =
         build_fcn_call scope.fs_vars typemap pos "llvm.x86.xbegin" []
       in
       let e = Expr_Binary (OperEquals, false,
                            DefTypePrimitive (PrimI32, [Volatile]),
                            Expr_Literal (LitI32 (Int32.of_int (-1))), f)
       in
       let xact_bb = make_sequential_bb ("xact_" ^ pos_label) [] in
       let bb = make_conditional_bb ("xbegin_" ^ pos_label) (pos, e) in
       let body_scope = push_scope ~xaction:true scope in
       let body_end_bb = process_bb body_scope xact_bb body in
       let _, f =
         build_fcn_call scope.fs_vars typemap pos "llvm.x86.xend" []
       in
       let xend_bb = make_sequential_bb ("xend." ^ pos_label) [(pos, f)] in
       begin
         add_next prev_bb bb;
         add_next bb xact_bb;
         add_next bb bb;
         add_next body_end_bb xend_bb;
         process_bb scope xend_bb rest
       end
    | IfStmt (pos, cond, then_block, else_block_maybe) :: rest ->
       let _, cexpr = convert_expr typemap scope cond in
       let succ_bb = make_sequential_bb ("succ_" ^ (label_of_pos pos)) [] in
       let fail_bb = make_sequential_bb ("fail_" ^ (label_of_pos pos)) [] in
       let cond_bb = generate_conditional ("cond_" ^ (label_of_pos pos))
         pos cexpr succ_bb fail_bb
       in
       let () = add_next prev_bb cond_bb in
       let then_bb = process_bb (push_scope scope) succ_bb then_block in
       let merge_bb = make_sequential_bb ("merge_" ^ (label_of_pos pos)) [] in
       let () = add_next then_bb merge_bb in
       let () = match else_block_maybe with
         | None -> add_next fail_bb merge_bb
         | Some stmts ->
            let else_bb = process_bb (push_scope scope) fail_bb stmts in
            add_next else_bb merge_bb
       in
       process_bb scope merge_bb rest
    | ForLoop (pos, (*parallel=*)false, init, (_, cond), iter, body) :: rest ->
       (* FIXME: Should really unify this with parallel for loop case. *)
       let init_scope = push_scope ~cont:None ~break:None scope in
       let init_bb = match init with
         | None -> prev_bb
         | Some stmt_or_decl -> process_bb init_scope prev_bb [stmt_or_decl]
       in
       let succ_bb = make_sequential_bb ("fsucc." ^ (label_of_pos pos)) [] in
       let fail_bb = make_sequential_bb ("ffail." ^ (label_of_pos pos)) [] in
       let _, cexpr = convert_expr typemap init_scope cond in
       let cond_bb = generate_conditional ("for_" ^ (label_of_pos pos))
                                          pos cexpr succ_bb fail_bb in
       let iter_bb = match iter with
         | None -> cond_bb
         | Some (pos, e) ->
            let _, iexpr = convert_expr typemap init_scope e in
            let bb =
              make_sequential_bb ("fiter_" ^ (label_of_pos pos)) [(pos, iexpr)]
            in
            let () = add_next bb cond_bb in
            bb
       in
       let () = add_next prev_bb init_bb in
       let () = add_next init_bb cond_bb in
       let body_scope =
         push_scope ~cont:(Some iter_bb) ~break:(Some fail_bb) init_scope in
       let for_body_end = process_bb body_scope succ_bb body in
       let () = add_next for_body_end iter_bb in
       process_bb scope fail_bb rest
    | ForLoop (pos, (*parallel=*)true, init, (_, cond), iter, body) :: rest ->
       let init_scope = push_scope ~cont:None ~break:None scope in
       let init_bb = match init with
         | None -> prev_bb
         | Some stmt_or_decl -> process_bb init_scope prev_bb [stmt_or_decl]
       in
       let for_region = (label_of_pos pos) ^ "." ^ (Util.unique_id ()) in
       let () = push_sync_region for_region in
       let succ_bb = make_detach_bb ("pfdetach." ^ (label_of_pos pos))
                                    for_region [] None in
       let fail_bb = make_sequential_bb ("pffail." ^ (label_of_pos pos)) [] in
       let _, cexpr = convert_expr typemap init_scope cond in
       let cond_bb = generate_conditional ("parfor." ^ (label_of_pos pos))
                                          pos cexpr succ_bb fail_bb in
       let () = set_cond_parallel cond_bb in
       let iter_bb = match iter with
         | None -> Report.err_parfor_needs_iter pos
         | Some (pos, e) ->
            let _, iexpr = convert_expr typemap init_scope e in
            let bb =
              make_sequential_bb ("pfiter." ^ (label_of_pos pos))
                                 [(pos, iexpr)]
            in
            let () = add_next bb cond_bb in
            bb
       in
       let () = add_next prev_bb init_bb in
       let () = add_next init_bb cond_bb in
       let internal_sync = scope.fs_sync_label ^ "." ^ (Util.unique_id ()) in
       let body_scope =
         push_scope ~cont:(Some iter_bb) ~break:None ~sync:(Some internal_sync)
                    init_scope
       in
       let for_body_end = process_bb body_scope succ_bb body in
       let () = add_next succ_bb iter_bb in
       let reattach = make_reattach_bb ((label_of_pos pos) ^ ".reattach")
                                       for_region [] in
       let () = add_next for_body_end reattach in
       let () = add_next reattach iter_bb in
       let sync_block = make_sync_bb ((label_of_pos pos) ^ ".sync") for_region
       in
       let () = add_next fail_bb sync_block in
       process_bb scope sync_block rest
    | WhileLoop (pos, precheck, cond, body) :: rest ->
       let _, cexpr = convert_expr typemap scope cond in
       let succ_bb = make_sequential_bb ("succ_" ^ (label_of_pos pos)) [] in
       let cond_bb, fail_bb =
         match (loop_can_exit cexpr)
               || (can_escape_forward label_bbs body) with
         | true ->
            let fail = make_sequential_bb ("fail_" ^ (label_of_pos pos)) [] in
            let cond = generate_conditional ("while_" ^ (label_of_pos pos))
              pos cexpr succ_bb fail in
            (* FIXME: Are the next two lines redundant? *)
            let () = add_next cond succ_bb in
            let () = add_next cond fail in
            cond, fail
         | false ->
            let cond = make_sequential_bb ("loop_" ^ (label_of_pos pos)) [] in
            let () = add_next cond succ_bb in
            cond, cond
       in
       let body_begin =
         make_sequential_bb ("loop_body_" ^ (label_of_pos pos)) [] in
       let body_scope =
         push_scope ~cont:(Some cond_bb) ~break:(Some fail_bb) scope in
       if precheck then
         let () = add_next prev_bb cond_bb in
         let () = add_next succ_bb body_begin in
         let body_end = process_bb body_scope body_begin body in
         let () = add_next body_end cond_bb in
         process_bb scope fail_bb rest
       else
         let () = add_next prev_bb body_begin in
         let body_end = process_bb body_scope body_begin body in
         let () = add_next body_end cond_bb in
         let () = add_next succ_bb body_begin in
         process_bb scope fail_bb rest
    | SwitchStmt (pos, expr, cases) :: rest ->
       (* FIXME: Treating this as an if-statement, but it should be special-
          cased for traditional, C-like switch statements. *)
       let label = label_of_pos pos in
       let switch_tp, switch_expr = convert_expr typemap scope expr in
       let switch_var_nm = "switch_var." ^ (Util.unique_id ()) in
       let switch_decl = make_decl pos scope switch_var_nm switch_tp in
       let () = add_decl switch_var_nm switch_decl in
       let switch_assign = Expr_Binary (OperAssign, false, switch_tp,
                                        Expr_Variable (switch_var_nm),
                                        switch_expr) in
       let switch_var = Expr_Variable switch_var_nm in
       let switch_bb = make_sequential_bb ("switch." ^ label)
                                          [pos, switch_assign] in
       let exit_bb = make_sequential_bb ("switch_exit." ^ label) [] in
       let reconcile pos l r =
         let _, reconciled_tp, left, right =
           binary_reconcile typemap pos OperEquals l r
         in
         Expr_Binary (OperEquals, false, reconciled_tp, left, right)
       in
       let rec get_member_types = function
         | DefTypeNamedStruct nm ->
            get_member_types (Util.the @@ lookup_symbol typemap nm)
         | DefTypeLiteralStruct (mtypes, _) -> mtypes
         | DefTypeStaticStruct mtypes -> mtypes
         | _ -> Report.err_internal __FILE__ __LINE__ "No members"
       in
       let get_array_type = function
         | DefTypeArray (tp, _) -> tp
         | _ -> Report.err_internal __FILE__ __LINE__ "Not an array."
       in
       let make_ands atoms =
         List.fold_left
           (fun accum expr ->
             Expr_Binary (OperLogicalAnd, false,
                          DefTypePrimitive (PrimBool, []),
                          accum, expr))
           (List.hd atoms) (List.tl atoms)
       in
       let wildcard_match pos switch_expr case_expr =
         let rec wildcards = function
           | _, (_, Expr_Wildcard) -> Expr_Literal (LitBool true)
           | (ltype, lexpr),
             (rtype, Expr_StaticStruct (_, rmembers)) ->
              let mtypes = get_member_types ltype in
              let vtypes, vexprs = List.split rmembers in
              let equals =
                List.mapi
                  (fun n (mtype, vtype, vexpr) ->
                    let member = Expr_SelectField (lexpr, n, false) in
                    wildcards ((mtype, member), (vtype, vexpr)))
                  (combine3 mtypes vtypes vexprs)
              in
              make_ands equals
           | (ltype, lexpr), (rtype, Expr_StaticArray elements) ->
              let subtype = get_array_type rtype in
              let equals =
                List.mapi
                  (fun n element ->
                    let expr_n = Expr_Literal (LitI32 (Int32.of_int n)) in
                    let el_n = Expr_Index (lexpr, expr_n, subtype, true, false) in
                    wildcards ((subtype, el_n), (subtype, element)))
                  elements
              in
              make_ands equals
           | (ltype, lexpr), (rtype, rexpr) ->
              reconcile pos (ltype, lexpr) (rtype, rexpr)
         in
         wildcards (switch_expr, case_expr)
       in

       let make_case (last_bb, fallthrough_bb)
                     (case_pos, case_expr, stmts) =
         let case_label = "case." ^ (label_of_pos case_pos) in
         let case_tp, case_conv = convert_expr typemap scope case_expr in
         let case_cmp = wildcard_match case_pos
                                       (switch_tp, switch_var)
                                       (case_tp, case_conv)
         in
         let succ_bb = make_sequential_bb (case_label ^ ".succ") [] in
         let fail_bb = make_sequential_bb (case_label ^ ".fail") [] in
         let cond_bb = generate_conditional case_label case_pos case_cmp
                                            succ_bb fail_bb
         in
         let case_scope = push_scope ~break:(Some exit_bb) scope in
         let case_end = process_bb case_scope succ_bb stmts in
         begin
           add_next last_bb cond_bb;
           if fallthrough_bb <> None then
             add_next (Util.the fallthrough_bb) succ_bb;
           match List.hd (List.rev stmts) with
           | Return _
           | ReturnVoid _
           | Goto _
           | Break _
           | Continue _ ->
              fail_bb, None
           | _ ->
              fail_bb, Some case_end
         end
       in
       let last_bb, fallthrough =
         List.fold_left make_case (switch_bb, None) cases
       in
       begin
         add_next prev_bb switch_bb;
         add_next last_bb exit_bb;
         if fallthrough <> None then
           add_next (Util.the fallthrough) exit_bb;
         process_bb scope exit_bb rest
       end
    | Return (pos, expr) :: rest ->
       let tp, expr = convert_expr typemap scope expr in
       begin
         check_castability pos typemap tp ret_type;
         match tp, expr with
         | DefTypeStaticStruct _, Expr_StaticStruct (_, elist) ->
            let nm = "defret." ^ (Util.unique_id ()) in
            let decl = make_decl pos scope nm tp in
            let () = add_decl nm decl in
            let structvar = Expr_Variable nm in
            let exprs =
              List.mapi (fun n (t, e) ->
                pos,
                Expr_Binary (OperAssign, false, t,
                             (* volatility doesn't matter, here, since it's
                                the lhs of an assignment. => false *)
                             Expr_SelectField (structvar, n, false),
                             e))
                elist
            in
            let expr_bb =
              make_sequential_bb ("static_" ^ (label_of_pos pos)) exprs in
            let () = add_next prev_bb expr_bb in
            let cast = maybe_cast typemap tp ret_type structvar in
            let term_bb = make_terminal_bb ("ret_" ^ (label_of_pos pos))
                                           scope.fs_in_xaction
                                           (Some (pos, cast)) in
            let () = add_next expr_bb term_bb in
            process_bb scope term_bb rest
         | _ ->
            let cast = maybe_cast typemap tp ret_type expr in
            let term_bb = make_terminal_bb ("ret_" ^ (label_of_pos pos))
                                           scope.fs_in_xaction
                                           (Some (pos, cast)) in
            let () = add_next prev_bb term_bb in
            process_bb scope term_bb rest
       end
    | ReturnVoid pos :: rest ->
       let term_bb = make_terminal_bb ("ret_" ^ (label_of_pos pos))
                                      scope.fs_in_xaction
                                      None in
       let () = add_next prev_bb term_bb in
       process_bb scope term_bb rest
    | TypeDecl _ :: _ ->
       Report.err_internal __FILE__ __LINE__
         "FIXME: TypeDecl not implemented Cfg.build_bbs (TypeDecl)"
    | Label (_, label) :: rest ->
       let bb = make_sequential_bb label [] in
       let () = Hashtbl.add label_bbs label bb in
       let () = add_next prev_bb bb in
       process_bb scope bb rest
    | Goto (pos, label) :: rest ->
       let bb = make_goto_bb label in
       let () = add_next prev_bb bb in
       process_bb scope bb rest
    | Break pos :: _ ->
       begin match scope.fs_break_bb with
       | None -> Report.err_no_break_scope pos
       | Some bb ->
          let () = add_next prev_bb bb in
          prev_bb
       end
    | Continue pos :: rest ->
       begin match scope.fs_cont_bb with
       | None ->
          Report.err_internal __FILE__ __LINE__
            "Tried to continue without an enclosing loop scope."
       | Some condition ->
          let bb = make_sequential_bb ("cont_" ^ (label_of_pos pos)) [] in
          let () = add_next prev_bb bb in
          let () = add_next bb condition in
          process_bb scope bb rest
       end
    | Sync pos :: rest ->
       let bb =
         make_sync_bb ("sync_" ^ (label_of_pos pos)) scope.fs_sync_label in
       let () = add_next prev_bb bb in
       process_bb scope bb rest
    | Import (tok, _) :: _ ->
       Report.err_import_in_function tok.td_pos name
  in
  let replace_gotos table () = function
    | BB_Goto (label, block) as old_bb ->
       let replacement = BB_Seq ("goto_stmt", block) in
       let replace_in_prev = function
         | BB_Seq (_, blk) -> blk.seq_next <- replacement
         | BB_Cond (_, blk) ->
            if blk.cond_next == old_bb then blk.cond_next <- replacement
            else blk.cond_else <- replacement
         | _ -> Report.err_internal __FILE__ __LINE__
            "Unexpected bb type in CFG."
       in
       let () = List.iter replace_in_prev block.seq_prev in
       let next = Hashtbl.find table label in
       block.seq_next <- next
    | _ -> ()
  in
  let entry_bb = make_sequential_bb "entry" [] in
  let fcnscope = make_fcn_scope scoped_vars in
  let _ = process_bb fcnscope entry_bb body in
  let bb_table = Hashtbl.create 32 in
  let () = visit_df (fun () bb -> match bb with
    | BB_Seq (label, _)
    | BB_Cond (label, _)
    | BB_Term (label, _) -> Hashtbl.add bb_table label bb
    | _ -> ())
    true
    ()
    entry_bb
  in
  let () = visit_df (replace_gotos bb_table) false () entry_bb in
  (*let () = Hashtbl.iter fixup_wildcards bb_table in *)
  unique_regions (), List.rev !decls, entry_bb

let verify_cfg name =
  let verify () = function
    | BB_Seq _
    | BB_Cond _
    | BB_Term _
    | BB_Detach _
    | BB_Reattach _
    | BB_Sync _ -> ()
    | BB_Goto (label, _) ->
       Report.err_internal __FILE__ __LINE__
         ("Found BB_Goto " ^ label ^ " in " ^ name)
    | BB_Error ->
       Report.err_internal __FILE__ __LINE__ ("Found BB_Error in " ^ name)
  in
  visit_df verify true ()

let build_global_vars decltable typemap globals = function
  | VarDecl (vars, inits, _) ->
     let procvar accum (var, init) =
       if init = None then accum
       else
         let decl = Util.the (lookup_symbol decltable var.td_text) in
         let scope = make_fcn_scope decltable in
         let etp, expr = convert_expr typemap scope (Util.the init) in
         (decl.mappedname, maybe_cast typemap etp decl.tp expr) :: accum
     in
     let varset = if inits = [] then List.map (fun v -> v, None) vars
                  else List.map (fun (v, i) -> v, Some i)
                                (List.combine vars inits)
     in
     List.fold_left procvar globals varset
  | _ -> globals

let build_fcns decltable typemap fcns = function
  | DefFcn (pos, _, _, name, _, body) ->
     let syncs, decls, entry_bb = build_bbs name decltable typemap body in
     let () = verify_cfg name entry_bb in
     let () = reset_bbs entry_bb in
     let fcn =
       { defn_begin = pos;
         defn_end = pos;
         name = name;
         local_vars = decls;
         entry_bb = entry_bb;
         fcn_cilk_init = syncs
       }
     in fcn :: fcns
  | _ -> fcns

let builtin_types () =
  let map = make_symtab () in
  List.iter (fun (name, tp, _, _) ->
    add_symbol map name tp)
    Types.map_builtin_types;
  map

let resolve_builtins stmts typemap =
  let rec process_expr = function
    | ExprFcnCall f ->
       begin match f.fc_name with
         | "sizeof" ->
            begin match f.fc_args with
            | [ ExprType (p, tp) ] ->
               make_size_expr typemap p tp
            | _ :: [] -> Report.err_internal __FILE__ __LINE__
               "FIXME: No error message for this."
            | _ -> Report.err_internal __FILE__ __LINE__
               "FIXME: No error message for this."
            end
         | "cast" ->
            begin match f.fc_args with
            | [ ExprType (p, tp); e ] ->
               ExprCast (p, tp, process_expr e)
            | _ -> Report.err_bad_args_for_builtin f.fc_pos "cast"
            end
         | "typestr" ->
            begin match f.fc_args with
            | [ expr ] -> ExprTypeString (f.fc_pos, expr)
            | _ -> Report.err_bad_args_for_builtin f.fc_pos "typestr"
            end
         | _ ->
            ExprFcnCall
              { fc_pos = f.fc_pos;
                fc_name = f.fc_name;
                fc_template = f.fc_template;
                fc_args = List.map process_expr f.fc_args;
                fc_spawn = f.fc_spawn
              }
       end
    | ExprBinary op ->
       let newop =
         { op_pos = op.op_pos;
           op_op = op.op_op;
           op_left = process_expr op.op_left;
           op_right = Some (process_expr (Util.the op.op_right));
           op_atomic = op.op_atomic
         }
       in ExprBinary newop
    | ExprPreUnary op ->
       let newop =
         { op_pos = op.op_pos;
           op_op = op.op_op;
           op_left = process_expr op.op_left;
           op_right = None;
           op_atomic = op.op_atomic
         }
       in ExprPreUnary newop
    | ExprPostUnary op ->
       let newop =
         { op_pos = op.op_pos;
           op_op = op.op_op;
           op_left = process_expr op.op_left;
           op_right = None;
           op_atomic = op.op_atomic
         }
       in ExprPostUnary newop
    | ExprCast (p, v, e) ->
       ExprCast (p, v, process_expr e)
    | ExprIndex (p1, base, p2, idx) ->
       ExprIndex (p1, process_expr base, p2, process_expr idx)
    | ExprSelectField (p1, p2, e, field) ->
       ExprSelectField (p1, p2, process_expr e, field)
    | ExprStaticStruct (p, elist) ->
       ExprStaticStruct (p, List.map (fun (p, e) -> p, process_expr e) elist)
    | ExprStaticArray (p, elements) ->
       ExprStaticArray (p, List.map (fun (p, e) -> p, process_expr e) elements)
    | e -> e
  in
  let rec process_stmt = function
    | StmtExpr (p, e) -> StmtExpr (p, process_expr e)
    | Block (p, stmts) -> Block (p, List.map process_stmt stmts)
    | DefFcn (p, doc, vis, name, tp, stmts) ->
       DefFcn(p, doc, vis, name, tp, List.map process_stmt stmts)
    | VarDecl (vars, inits, tp) ->
       VarDecl (vars, List.map process_expr inits, tp)
    | InlineStructVarDecl (p, vars, (epos, expr)) ->
       InlineStructVarDecl (p, vars, (epos, process_expr expr))
    | IfStmt (p, cond, tstmts, estmts_maybe) ->
       IfStmt (p, process_expr cond,
               List.map process_stmt tstmts,
               if estmts_maybe = None then None
               else Some (List.map process_stmt (Util.the estmts_maybe)))
    | ForLoop (p, is_parallel, init, cond, iter, stmts) ->
       let newinit = match init with
         | None -> None
         | Some s -> Some (process_stmt s)
       and newcond =
         let p, e = cond in p, process_expr e
       and newiter = match iter with
         | None -> None
         | Some (p, e) -> Some (p, process_expr e)
       in
       ForLoop (p, is_parallel, newinit, newcond, newiter,
                List.map process_stmt stmts)
    | WhileLoop (p, pre, cond, stmts) ->
       WhileLoop (p, pre, process_expr cond, List.map process_stmt stmts)
    | SwitchStmt (p, expr, cases) ->
       let process_case (pos, ctor, stmts) =
         pos, process_expr ctor, List.map process_stmt stmts
       in
       SwitchStmt (p, process_expr expr, List.map process_case cases)
    | Return (p, e) -> Return (p, process_expr e)
    | stmt -> stmt
  in
  List.map process_stmt stmts

let of_ast stmts =
  let typemap = builtin_types () in
  let decltable = make_symtab () in
  List.iter (global_types typemap) stmts;
  let typemap = symtab_filter (resolve_type typemap) typemap in
  let sanitized_stmts = resolve_builtins stmts typemap in
  List.iter (global_decls decltable typemap) sanitized_stmts;
  let global_vars =
    List.fold_left (build_global_vars decltable typemap) [] sanitized_stmts
  in
  let fcnlist =
    List.fold_left (build_fcns decltable typemap) [] sanitized_stmts
  in
  let initializers = Hashtbl.create 16 in
  let () = List.iter (fun (nm, e) -> Hashtbl.add initializers nm e) global_vars
  in
  { global_decls = decltable;
    initializers = initializers;
    fcnlist = List.rev fcnlist;
    deftypemap = typemap
  }
