open Ast
open Lexing
open Types
open Util

type cfg_expr =
  | Expr_FcnCall of string * cfg_expr list
  | Expr_Binary of Ast.operator * cfg_expr * cfg_expr
  | Expr_Unary of Ast.operator * cfg_expr * bool
  | Expr_Literal of Ast.literal
  | Expr_Variable of string
  | Expr_Cast of Types.deftype * Types.deftype * cfg_expr
  | Expr_Index of cfg_expr * cfg_expr
  | Expr_SelectField of cfg_expr * int

type cfg_basic_block =
  | BB_Cond of conditional_block
  | BB_Loop of loop_block
  | BB_Expr of Lexing.position * cfg_expr
  | BB_Return of Lexing.position * cfg_expr
  | BB_ReturnVoid of Lexing.position

and conditional_block =
  { if_pos       : Lexing.position;
    fi_pos       : Lexing.position;
    branch_cond  : cfg_expr;

    then_scope   : cfg_basic_block list;
    then_returns : bool;

    else_scope   : cfg_basic_block list;
    else_returns : bool
  }

and loop_block =
  { while_pos  : Lexing.position;
    precheck   : bool;
    loop_cond  : cfg_expr;
    body_scope : cfg_basic_block list;
  }

and decl =
  { decl_pos   : Lexing.position;
    mappedname : string;
    tp         : Types.deftype;
    params     : (Lexing.position * string) list (* Zero-length for non-fcns *)
  }

type function_defn =
  { defn_begin : Lexing.position;
    defn_end   : Lexing.position;
    name       : string;
    local_vars : (string * decl) list;
    bbs        : cfg_basic_block list
  }

type program =
  { global_decls : decl Util.symtab;
    fcnlist : function_defn list;
    deftypemap : deftype symtab
  }

(** Get the type of an Ast.literal value. *)
let typeof_literal lit =
  let t = function
    | LitBool _ -> PrimBool
    | LitI16 _ -> PrimI16
    | LitU16 _ -> PrimU16
    | LitI32 _ -> PrimI32
    | LitU32 _ -> PrimU32
    | LitI64 _ -> PrimI64
    | LitU64 _ -> PrimU64
  in DefTypePrimitive (t lit)

let rec convert_type defining_p typemap = function
  | VarType (pos, name) ->
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
       | Some t -> t
     end
  | FcnType (params, ret) ->
     DefTypeFcn
       (List.map (fun (_, _, tp) ->
         convert_type defining_p typemap tp) params,
        convert_type defining_p typemap ret)
  | StructType elements ->
     let process (names, deftypes) (_, name, tp) =
       (name :: names, (convert_type defining_p typemap tp) :: deftypes)
     in
     let names, deftypes = List.fold_left process ([], []) elements in
     DefTypeLiteralStruct (List.rev deftypes, List.rev names)
  | PtrType (pos, tp) -> DefTypePtr (convert_type defining_p typemap tp)

let param_pos_names = function
  | VarType _ -> []
  | PtrType _ -> []
  | StructType _ -> []
  | FcnType (params, _) ->
     List.map (fun (pos, name, _) -> (pos, name)) params

let global_types typemap = function
  | TypeDecl (pos, name, tp) ->
     (* FIXME: When structs get added, check tp to see if it's a struct
        and add a symbolic reference, first, before converting the type. *)
     add_symbol typemap name (convert_type true typemap tp)
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
    | DefTypeFcn (params, ret) ->
       let params = List.map v params
       and ret = v ret
       in DefTypeFcn (params, ret)
    | DefTypePtr tp -> DefTypePtr (v tp)
    | DefTypeLiteralStruct (fields, names) ->
       DefTypeLiteralStruct (List.map v fields, names)
  in Some (v oldtp)

let global_decls decltable typemap = function
  | DefFcn (pos, name, tp, _) ->
     begin match lookup_symbol decltable name with
     | Some _ -> ()
     | None ->
        let fcn =
          { decl_pos = pos;
            mappedname = name;
            tp = convert_type false typemap tp;
            params = param_pos_names tp }
        in
        add_symbol decltable name fcn
     end
  | TypeDecl _ -> ()
  | _ -> Report.err_internal __FILE__ __LINE__
     "FIXME: Incomplete implementation of Cfg.global_decls."

let get_fcntype_profile = function
  | DefTypeFcn (params, ret) -> params, ret
  | _ -> Report.err_internal __FILE__ __LINE__ " Unexpected function type."

(** Return true iff the series of Ast.stmts returns on all paths. *)
let rec returns_p stmts =
  let r ret = function
    | StmtExpr _ -> ret
    | Block (_, stmts) -> (returns_p stmts) || ret
    | DefFcn _ -> Report.err_internal __FILE__ __LINE__
       "FIXME: Unhandled case in Cfg.returns_p (DefFcn)"
    | VarDecl _ -> ret
    | IfStmt (_, _, t, None) -> ret
    | IfStmt (_, _, t, Some e) ->
       ((returns_p t) && (returns_p e)) || ret
    | WhileLoop _ -> ret
    | Return _ -> true
    | ReturnVoid _ -> true
    | TypeDecl _ -> Report.err_internal __FILE__ __LINE__
       "FIXME: Unhandled case in Cfg.returns_p (TypeDecl)"
  in List.fold_left r false stmts

(** Return a casted version of the expression, if the original type doesn't
    match the desired type. *)
let maybe_cast orig cast_as expr =
  if orig = cast_as then expr
  else Expr_Cast (orig, cast_as, expr)

let check_castability pos ltype rtype =
  let rec similar = function
    | DefTypePrimitive lprim, DefTypePrimitive rprim ->
       ()  (* FIXME: Implement. *)
    | DefTypeFcn (plist1, ret1), DefTypeFcn (plist2, ret2) ->
       begin
         List.iter identical (List.combine plist1 plist2);
         identical (ret1, ret2)
       end
    | DefTypePtr p1, DefTypePtr p2 ->
       identical (p1, p2)
    | _ -> failwith "FIXME: check_castability incomplete."
  and identical (ltype, rtype) =
    if ltype = rtype then ()
    else Report.err_type_mismatch pos
  in
  similar (ltype, rtype)

(** Reconcile the types of two subexpressions connected by a binary operator
    and return the result.  The result may include implicit casts. *)
let binary_reconcile =
  let more_general_of pos op ltype rtype =
    match ltype, rtype with
    | DefTypePrimitive lprim, DefTypePrimitive rprim ->
       DefTypePrimitive (generalize_primitives lprim rprim)
    | _ -> failwith "FIXME: more_general_of incomplete."
  in
  let reconcile pos op (ltype, lexpr) (rtype, rexpr) =
    match op with
    | OperPlus | OperMinus | OperMult | OperDiv ->
       let tp = more_general_of pos op ltype rtype in
       tp, (maybe_cast ltype tp lexpr), (maybe_cast rtype tp rexpr)
    | OperLT | OperLTE
    | OperGT | OperGTE
    | OperEquals | OperNEquals ->
       let tp = more_general_of pos op ltype rtype in
       DefTypePrimitive PrimBool,
       (maybe_cast ltype tp lexpr),
       (maybe_cast rtype tp rexpr)
    | OperAssign ->
       begin
         check_castability pos rtype ltype;
         ltype, lexpr, (maybe_cast rtype ltype rexpr)
       end
    | _ -> Report.err_internal __FILE__ __LINE__
       "FIXME: Incomplete implementation Cfg.reconcile."
  in reconcile

let build_fcn_call scope pos name args =
  match lookup_symbol scope name with
  | None -> Report.err_unknown_fcn_call pos name
  | Some decl ->
     let match_param_with_arg ptype (atype, expr) =
       check_castability pos atype ptype;
       maybe_cast atype ptype expr
     in
     begin match decl.tp with
     | DefTypeUnresolved _ -> Report.err_internal __FILE__ __LINE__
        "Tried to call an unresolved type."
     | DefTypeNamedStruct _ -> Report.err_internal __FILE__ __LINE__
        "Tried to call a named-struct-type."
     | DefTypeVoid -> Report.err_internal __FILE__ __LINE__
        "Tried to call a void."
     | DefTypeFcn (params, rettp) ->
        begin
          try
            let casted_args = List.map2 match_param_with_arg params args in
            rettp, decl.mappedname, casted_args
          with _ ->
            Report.err_wrong_number_of_args pos decl.decl_pos name
              (List.length params) (List.length args)
        end
     | DefTypePrimitive _ -> Report.err_called_non_fcn pos decl.decl_pos name
     | DefTypePtr _ -> Report.err_internal __FILE__ __LINE__
        "Tried to call a pointer."
     | DefTypeLiteralStruct _ -> Report.err_internal __FILE__ __LINE__
        "Tried to call a struct."
     end

let convert_expr typemap scope =
  let rec convert = function
    | ExprFcnCall call ->
       let converted_args = List.map convert call.fc_args in
       let rettp, fcn, cfg_args =
         build_fcn_call scope call.fc_pos call.fc_name converted_args
       in rettp, Expr_FcnCall (fcn, cfg_args)
    | ExprBinary bo ->
       let tp, lhs, rhs =
         binary_reconcile bo.bo_op_pos bo.bo_op
           (convert bo.bo_left) (convert bo.bo_right)
       in tp, Expr_Binary (bo.bo_op, lhs, rhs)
    | ExprVar (pos, name) ->
       let var = the (lookup_symbol scope name)
       in var.tp, Expr_Variable var.mappedname (* FIXME! Wrong type. *)
    | ExprLit (pos, literal) ->
       (typeof_literal literal), Expr_Literal literal
    | ExprIndex (bpos, base, ipos, idx) ->
       let btype, converted_base = convert base
       and itype, converted_idx = convert idx
       in begin match btype with
       | DefTypePtr DefTypeVoid -> Report.err_deref_void_ptr bpos ipos
       | DefTypePtr deref_type ->
          if is_integer_type itype then
            deref_type, Expr_Index (converted_base, converted_idx)
          else
            Report.err_non_integer_index ipos
       | _ -> Report.err_index_non_ptr ipos
       end
    | ExprSelectField (dpos, fpos, obj, fieldname) ->
       let otype, converted_obj = convert obj in
       let rec struct_select obj = function
         | DefTypeLiteralStruct (mtypes, fields) ->
            let rec get_field n = function
              | [] -> Report.err_struct_no_such_member fpos fieldname
              | f :: rest ->
                 if f = fieldname then n else get_field (n + 1) rest
            in
            let n = get_field 0 fields in
            List.nth mtypes n, Expr_SelectField (obj, n)
         | DefTypeNamedStruct sname ->
            struct_select obj (the (lookup_symbol typemap sname))
         | DefTypePtr p ->
            let idx = LitI32 (Int32.of_int 0) in
            let derefed_obj = Expr_Index (obj, Expr_Literal idx) in
            struct_select derefed_obj p
         | _ -> Report.err_non_struct_member_access dpos
       in
       struct_select converted_obj otype
    | _ -> Report.err_internal __FILE__ __LINE__
       "FIXME: Cfg.convert_expr not fully implemented."
  in convert

let nonconflicting_name pos scope name =
  match lookup_symbol_local scope name with
  | None -> begin match lookup_symbol scope name with
    | None -> name
    | _ ->
       "_def_" ^ name ^ "_" ^ (string_of_int pos.pos_lnum)
  end
  | Some decl -> Report.err_redeclared_variable pos decl.decl_pos name

let build_bbs name decltable typemap body =
  let fcndecl = the (lookup_symbol decltable name) in
  let param_types, ret_type = get_fcntype_profile fcndecl.tp in

  (* Add the function's parameters to the scope table. *)
  let fcnscope = push_symtab_scope decltable in
  List.iter2 (fun tp (pos, name) ->
    add_symbol fcnscope name
      { decl_pos = pos; mappedname = name; tp = tp; params = [] })
    param_types
    fcndecl.params;

  let rec process_block scope decls =
    List.fold_left
      (process_bb (push_symtab_scope scope)) (decls, [])
  and process_bb scope (decls, bbs) = function
    | StmtExpr (pos, expr) ->
       let _, expr = convert_expr typemap scope expr in
       decls, BB_Expr (pos, expr) :: bbs
    | Block (_, stmts) -> (* FIXME: scope should shadow new variables. *)
       List.fold_left
         (process_bb (push_symtab_scope scope)) (decls, bbs) stmts
    | DefFcn _ ->
       Report.err_internal __FILE__ __LINE__
         "FIXME: DefFcn not implemented Cfg.build_bbs (DefFcn)"
    | VarDecl (pos, name, tp, initializer_maybe) ->
       let mappedname = nonconflicting_name pos scope name in
       let decl = { decl_pos = pos;
                    mappedname = mappedname;
                    tp = convert_type false typemap tp;
                    params = param_pos_names tp } in
       add_symbol scope name decl;
       begin match initializer_maybe with
       | None -> (mappedname, decl) :: decls, bbs
       | Some (pos, expr) ->
          (* FIXME: Need to cast type. *)
          let (_, expr) = convert_expr typemap scope expr in
          ((mappedname, decl) :: decls,
           BB_Expr (pos, expr) :: bbs)
       end
    | IfStmt (pos, cond, then_block, else_block_maybe) ->
       let (decls, else_scope), else_returns = match else_block_maybe with
         | None -> (decls, []), false
         | Some stmts -> process_block scope decls stmts, returns_p stmts
       in
       let decls, then_scope = process_block scope decls then_block in
       let tp, conv_cond = convert_expr typemap scope cond in
       let () = check_castability pos tp (DefTypePrimitive PrimBool) in
       let block =
         { if_pos = pos;
           fi_pos = pos; (* FIXME! *)
           branch_cond = maybe_cast tp (DefTypePrimitive PrimBool) conv_cond;
           then_scope = List.rev then_scope;
           then_returns = returns_p then_block;
           else_scope = List.rev else_scope;
           else_returns = else_returns
         }
       in
       decls, (BB_Cond block) :: bbs

    | WhileLoop (pos, precheck, cond, body) ->
       let decls, body_scope = process_block scope decls body in
       let tp, conv_cond = convert_expr typemap scope cond in
       let () = check_castability pos tp (DefTypePrimitive PrimBool) in
       let block =
         { while_pos = pos;
           precheck = precheck;
           loop_cond = maybe_cast tp (DefTypePrimitive PrimBool) conv_cond;
           body_scope = List.rev body_scope
         }
       in decls, (BB_Loop block) :: bbs

    | Return (pos, expr) ->
       let tp, expr = convert_expr typemap scope expr in
       begin
         check_castability pos tp ret_type;
         decls, BB_Return (pos, (maybe_cast tp ret_type expr)) :: bbs
       end
    | ReturnVoid pos ->
       if ret_type == DefTypeVoid then
         decls, BB_ReturnVoid pos :: bbs
       else
         Report.err_returned_void pos
    | TypeDecl _ ->
       Report.err_internal __FILE__ __LINE__
         "FIXME: DefFcn not implemented Cfg.build_bbs (TypeDecl)"
  in
  let decls, bbs = List.fold_left (process_bb fcnscope) ([], []) body in
  List.rev decls, List.rev bbs

let build_fcns decltable typemap fcns = function
  | DefFcn (pos, name, _, body) ->
     let decls, bbs = build_bbs name decltable typemap body in
     let fcn = { defn_begin = pos; defn_end = pos; name = name;
                 local_vars = decls; bbs = bbs }
     in fcn :: fcns
  | _ -> fcns

let builtin_types () =
  let map = make_symtab () in
  List.iter (fun (name, tp, _) ->
    add_symbol map name tp)
    Types.map_builtin_types;
  map

let convert_ast stmts =
  let typemap = builtin_types () in
  let decltable = make_symtab () in
  List.iter (global_types typemap) stmts;
  let typemap = symtab_filter (resolve_type typemap) typemap in
  List.iter (global_decls decltable typemap) stmts;
  let fcnlist = List.fold_left (build_fcns decltable typemap) [] stmts
  in
  { global_decls = decltable;
    fcnlist = List.rev fcnlist;
    deftypemap = typemap
  }
