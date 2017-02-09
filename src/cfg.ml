open Ast
open Lexing
open Types
open Util

type cfg_expr =
  | Expr_FcnCall of string * cfg_expr list
  | Expr_Binary of Ast.operator * Types.deftype * cfg_expr * cfg_expr
  | Expr_Unary of Ast.operator * Types.deftype * cfg_expr * (*pre_p*)bool
  | Expr_Literal of Ast.literal
  | Expr_Variable of string
  | Expr_Cast of Types.deftype * Types.deftype * cfg_expr
  | Expr_Index of cfg_expr * cfg_expr
  | Expr_SelectField of cfg_expr * int
  | Expr_StaticStruct of (Types.deftype * cfg_expr) list
  | Expr_Nil
  | Expr_Atomic of atomic_op * (Types.deftype * cfg_expr) list

and atomic_op =
  | AtomicCAS

type cfg_basic_block =
  | BB_Seq of string * sequential_block
  | BB_Cond of string * conditional_block
  | BB_Term of string * terminal_block
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
    mutable cond_mark_bit : bool
  }

and terminal_block =
  { mutable term_prev : cfg_basic_block list;
    term_expr         : (Lexing.position * cfg_expr) option;
    mutable term_mark_bit : bool
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
  }

type program =
  { global_decls : decl Util.symtab;
    fcnlist : function_defn list;
    deftypemap : Types.deftype Util.symtab
  }

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

(** Get the type of an Ast.literal value. *)
let typeof_literal lit =
  DefTypePrimitive (literal2primitive lit)

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
    | DefTypeNullPtr -> DefTypeNullPtr
    | DefTypeLiteralStruct (fields, names) ->
       DefTypeLiteralStruct (List.map v fields, names)
    | DefTypeStaticStruct members ->
       DefTypeStaticStruct (List.map v members)
  in Some (v oldtp)

let global_decls decltable typemap = function
  | DeclFcn (pos, vis, name, tp)
  | DefFcn (pos, vis, name, tp, _) ->
     begin match lookup_symbol decltable name with
     | Some _ -> ()
     | None ->
        let fcn =
          { decl_pos = pos;
            mappedname = name;
            vis = vis;
            tp = convert_type false typemap tp;
            params = param_pos_names tp
          }
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
let rec has_exit_p stmts =
  let r ret = function
    | StmtExpr _ -> ret
    | Block (_, stmts) -> (has_exit_p stmts) || ret
    | DeclFcn _
    | DefFcn _ -> Report.err_internal __FILE__ __LINE__
       "FIXME: Unhandled case in Cfg.has_exit_p (DefFcn)"
    | VarDecl _ -> ret
    | IfStmt (_, _, t, None) -> ret
    | IfStmt (_, _, t, Some e) ->
       ((has_exit_p t) && (has_exit_p e)) || ret
    | WhileLoop _ -> ret
    | Return _ -> true
    | ReturnVoid _ -> true
    | TypeDecl _ -> Report.err_internal __FILE__ __LINE__
       "FIXME: Unhandled case in Cfg.has_exit_p (TypeDecl)"
    | Label _ -> ret
    | Continue _ -> true
    | Goto _ -> true (* FIXME: unstructured goto is a little weird.  Think
                        about this some more... *)
  in List.fold_left r false stmts

(** Return a casted version of the expression, if the original type doesn't
    match the desired type. *)
let rec maybe_cast typemap orig cast_as expr =
  if orig = cast_as then expr
  else match cast_as with
  | DefTypeNamedStruct nm ->
     begin match the (lookup_symbol typemap nm) with
     | DefTypeLiteralStruct (tplist, _) ->
        begin match expr with
        | Expr_StaticStruct exprmembers ->
           let castmembers =
             List.map2
               (fun to_tp (from_tp, e) ->
                 to_tp, (maybe_cast typemap from_tp to_tp e))
               tplist
               exprmembers
           in
           Expr_Cast (orig, cast_as, Expr_StaticStruct castmembers)
        | _ ->
           Report.err_internal __FILE__ __LINE__ "Non-struct type struct."
        end
     | _ -> Report.err_internal __FILE__ __LINE__ "Non-struct type struct."
     end
  | _ -> Expr_Cast (orig, cast_as, expr)

(** Determine whether one type can be cast as another.  This function returns
    unit, as it only reports an error if it fails. *)
let check_castability pos typemap ltype rtype =
  let rec similar = function
    | DefTypePrimitive lprim, DefTypePrimitive rprim ->
       ()  (* FIXME: Implement. *)
    | DefTypeFcn (plist1, ret1), DefTypeFcn (plist2, ret2) ->
       begin
         List.iter identical (List.combine plist1 plist2);
         identical (ret1, ret2)
       end
    | DefTypePtr DefTypeVoid, DefTypePtr _
    | DefTypePtr _, DefTypePtr DefTypeVoid ->
       ()
    | DefTypePtr p1, DefTypePtr p2 ->
       identical (p1, p2)
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
    | DefTypePrimitive _, _ ->
       Report.err_internal __FILE__ __LINE__ (Util.format_position pos)
    | DefTypePtr _, DefTypeNullPtr
    | DefTypeNullPtr, DefTypePtr _ -> ()
    | _ -> Report.err_internal __FILE__ __LINE__
       ("incomplete cast implementation for " ^ (Util.format_position pos))
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
       tp, tp,
       (maybe_cast typemap ltype tp lexpr),
       (maybe_cast typemap rtype tp rexpr)
    | OperLT ->
       let tp = more_general_of pos op ltype rtype in
       DefTypePrimitive PrimBool,
       tp,
       (maybe_cast typemap ltype tp lexpr),
       (maybe_cast typemap rtype tp rexpr)
    | OperLTE
    | OperGT | OperGTE
    | OperEquals | OperNEquals
    | OperBitwiseAnd | OperBitwiseOr ->
       let tp = more_general_of pos op ltype rtype in
       tp,
       tp,
       maybe_cast typemap ltype tp lexpr,
       maybe_cast typemap rtype tp rexpr
    | OperLogicalOr | OperLogicalAnd ->
       let primbool = DefTypePrimitive PrimBool in
       begin
         check_castability pos typemap ltype primbool;
         check_castability pos typemap rtype primbool;
         primbool,
         primbool,
         maybe_cast typemap ltype primbool lexpr,
         maybe_cast typemap rtype primbool rexpr
       end
    | OperAssign ->
       begin
         (* FIXME: WORKING HERE -- Do automatic casting of static structs
            to named or literal structs, if possible.  Otherwise, casting
            doesn't work.  Literal structs need to be the correct type at
            instantiation. *)
         check_castability pos typemap ltype rtype;
         ltype, ltype, lexpr, (maybe_cast typemap rtype ltype rexpr)
       end
    | _ -> Report.err_internal __FILE__ __LINE__
       ("FIXME: Incomplete implementation Cfg.reconcile (operator "
        ^ (operator2string op) ^ ").")
  in reconcile

let build_fcn_call scope typemap pos name args =
  match lookup_symbol scope name with
  | None ->
     (* Check if the function is an atomic. *)
     begin match name with
     | "__builtin_cas" ->
        if (List.length args) <> 3 then
          Report.err_wrong_number_of_atomic_args pos name (List.length args)
        else
          DefTypePrimitive PrimBool, Expr_Atomic (AtomicCAS, args)
     | _ -> Report.err_unknown_fcn_call pos name
     end
  | Some decl ->
     let match_param_with_arg ptype (atype, expr) =
       check_castability pos typemap atype ptype;
       maybe_cast typemap atype ptype expr
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
            rettp, Expr_FcnCall (decl.mappedname, casted_args)
          with _ ->
            Report.err_wrong_number_of_args pos decl.decl_pos name
              (List.length params) (List.length args)
        end
     | DefTypePrimitive _ -> Report.err_called_non_fcn pos decl.decl_pos name
     | DefTypePtr _ -> Report.err_internal __FILE__ __LINE__
        "Tried to call a pointer."
     | DefTypeNullPtr -> Report.err_internal __FILE__ __LINE__
        "Tried to call nil."
     | DefTypeLiteralStruct _
     | DefTypeStaticStruct _ -> Report.err_internal __FILE__ __LINE__
        "Tried to call a struct."
     end

let convert_expr typemap scope =
  let rec convert = function
    | ExprFcnCall call ->
       let converted_args = List.map convert call.fc_args in
       build_fcn_call scope typemap call.fc_pos call.fc_name converted_args
    | ExprBinary op ->
       let rettp, tp, lhs, rhs =
         binary_reconcile typemap op.op_pos op.op_op
           (convert op.op_left) (convert (the op.op_right))
       in rettp, Expr_Binary (op.op_op, tp, lhs, rhs)
    | ExprPreUnary op ->
       let tp, subexpr = convert op.op_left in
       let rettp = match op.op_op with
         | OperAddrOf -> DefTypePtr tp
         | _ -> tp
       in
       rettp, Expr_Unary (op.op_op, tp, subexpr, true)
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
    | ExprCast (pos, to_tp, e) ->
       let cast_tp = convert_type false typemap to_tp in
       let orig_tp, converted_expr = convert e in
       if orig_tp = cast_tp then
         let () = prerr_endline
           ("omitting cast: " ^ (string_of_type orig_tp) ^ " -> "
            ^ (string_of_type cast_tp)
            ^ " (" ^ (format_position pos) ^ ")") in
         cast_tp, converted_expr
       else
         cast_tp, Expr_Cast (orig_tp, cast_tp, converted_expr)
    | ExprStaticStruct (_, members) ->
       let cmembers = List.map (fun (_, e) -> convert e) members in
       let tlist = List.rev (List.fold_left (fun taccum (t, _) ->
         (t :: taccum)) [] cmembers) in
       DefTypeStaticStruct tlist, Expr_StaticStruct cmembers
    | ExprType _ ->
       Report.err_internal __FILE__ __LINE__ "ExprType"
    | ExprNil _ -> DefTypeNullPtr, Expr_Nil
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

let make_decl pos scope nm tp =
  { decl_pos = pos;
    mappedname = nonconflicting_name pos scope nm;
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
                    cond_mark_bit = false
                  })

let make_terminal_bb label rexpr =
  BB_Term (label, { term_prev = [];
                    term_expr = rexpr;
                    term_mark_bit = false
                  })

let make_goto_bb label =
  BB_Goto (label, { seq_prev = [];
                    seq_next = BB_Error;
                    seq_expr = [];
                    seq_mark_bit = false
                  })

let rec build_bbs name decltable typemap body =
  let fcndecl = the (lookup_symbol decltable name) in
  let param_types, ret_type = get_fcntype_profile fcndecl.tp in

  (* Add the function's parameters to the scope table. *)
  let fcnscope = push_symtab_scope decltable in
  List.iter2 (fun tp (pos, name) ->
    add_symbol fcnscope name
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
      | BB_Seq (_, block) ->
         if block.seq_next = BB_Error then
           block.seq_next <- bb
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
      | BB_Goto (_, block) ->
         block.seq_prev <- bb :: block.seq_prev
      | BB_Cond (_, block) ->
         block.cond_prev <- bb :: block.cond_prev
      | BB_Term (_, block) ->
         block.term_prev <- bb :: block.term_prev
      | BB_Error -> ()
    in
    push_next bb2 bb1;
    push_prev bb1 bb2
  in

  (* Iterate through the basic blocks and convert them to the CFG structure
     along with all of the contained expressions.  This function will leave
     temporary placeholders, like BB_Goto and BB_Error, so the CFG needs
     cleanup. *)
  let rec process_bb scope decls prev_bb cont_bb = function
    | [] -> decls, prev_bb
    | StmtExpr (pos, expr) :: rest ->
       let _, expr = convert_expr typemap scope expr in
       let bb =
         make_sequential_bb ("expr_" ^ (label_of_pos pos)) [(pos, expr)] in
       let () = add_next prev_bb bb in
       process_bb scope decls bb cont_bb rest
    | Block (_, stmts) :: rest ->
       let decls, bb =
         process_bb (push_symtab_scope scope) decls prev_bb cont_bb stmts in
       process_bb scope decls bb cont_bb rest
    | DeclFcn _ :: _ ->
       Report.err_internal __FILE__ __LINE__ "DeclFcn not expected."
    | DefFcn _ :: _ ->
       Report.err_internal __FILE__ __LINE__ "DefFcn not supported."
    | VarDecl (vars, tp) :: rest ->
       let t = convert_type false typemap tp in
       let separate_vars (dlist, ilist) (pos, nm, init_maybe) =
         let decl = make_decl pos scope nm t in
         let () = add_symbol scope nm decl in
         let ilist = match init_maybe with
           | None -> ilist
           | Some (pos, expr) ->
              let oldtp, converted = convert_expr typemap scope expr in
              let cast_expr = maybe_cast typemap oldtp t converted in
              let assignment = Expr_Binary (OperAssign, t,
                                            Expr_Variable decl.mappedname,
                                            cast_expr) in
              (pos, assignment) :: ilist
         in
         (nm, decl) :: dlist, ilist
       in
       let dlist, ilist = List.fold_left separate_vars (decls, []) vars in
       let bb = match ilist with
         | (pos, _) :: _ ->
            let bb =
              make_sequential_bb ("initializers_" ^ (label_of_pos pos)) ilist
            in
            let () = add_next prev_bb bb in
            bb
         | [] -> prev_bb
       in
       process_bb scope (dlist @ decls) bb cont_bb rest
    | IfStmt (pos, cond, then_block, else_block_maybe) :: rest ->
       let _, cexpr = convert_expr typemap scope cond in
       let bb =
         make_conditional_bb ("cond_" ^ (label_of_pos pos)) (pos, cexpr) in
       let () = add_next prev_bb bb in
       let decls, then_bb =
         process_bb (push_symtab_scope scope) decls bb cont_bb then_block in
       let merge_bb = make_sequential_bb ("merge_" ^ (label_of_pos pos)) [] in
       let () = add_next then_bb merge_bb in
       let decls = match else_block_maybe with
         | None ->
            (add_next bb merge_bb; decls)
         | Some stmts ->
            let decls, else_bb =
              process_bb (push_symtab_scope scope) decls bb cont_bb stmts in
            (add_next else_bb merge_bb; decls)
       in
       process_bb scope decls merge_bb cont_bb rest
    | WhileLoop (pos, precheck, cond, body) :: rest ->
       let _, cexpr = convert_expr typemap scope cond in
       let cond_bb =
         if loop_can_exit cexpr then
           make_conditional_bb ("while_" ^ (label_of_pos pos)) (pos, cexpr)
         else
           make_sequential_bb ("loop_" ^ (label_of_pos pos)) []
       in
       let body_begin =
         make_sequential_bb ("loop_body_" ^ (label_of_pos pos)) [] in
       if precheck then
         let () = add_next prev_bb cond_bb in
         let () = add_next cond_bb body_begin in
         let decls, body_end =
           process_bb (push_symtab_scope scope) decls body_begin
             (Some cond_bb) body in
         let () = add_next body_end cond_bb in
         process_bb scope decls cond_bb cont_bb rest
       else
         let () = add_next prev_bb body_begin in
         let decls, body_end =
           process_bb (push_symtab_scope scope) decls body_begin
             (Some cond_bb) body in
         let () = add_next cond_bb body_begin in
         process_bb scope decls cond_bb cont_bb rest
    | Return (pos, expr) :: rest ->
       let tp, expr = convert_expr typemap scope expr in
       begin
         check_castability pos typemap tp ret_type;
         match tp, expr with
         | DefTypeStaticStruct _, Expr_StaticStruct elist ->
            let decl = { decl_pos = pos;
                         mappedname = "__defret"; (* FIXME: Unique name. *)
                         vis = VisLocal;
                         tp = tp;
                         params = []
                       } in
            let structvar = Expr_Variable "__defret" in
            let exprs =
              List.mapi (fun n (t, e) ->
                pos,
                Expr_Binary (OperAssign, t,
                             Expr_SelectField (structvar, n),
                             e))
                elist
            in
            let expr_bb =
              make_sequential_bb ("static_" ^ (label_of_pos pos)) exprs in
            let () = add_next prev_bb expr_bb in
            let retexpr = Expr_Variable "__defret" in
            let term_bb = make_terminal_bb
              ("ret_" ^ (label_of_pos pos))
              (Some (pos, (maybe_cast typemap tp ret_type retexpr))) in
            let () = add_next expr_bb term_bb in
            process_bb scope (("__defret", decl) :: decls) term_bb cont_bb rest
         | _ ->
            let term_bb = make_terminal_bb
              ("ret_" ^ (label_of_pos pos))
              (Some (pos, (maybe_cast typemap tp ret_type expr))) in
            let () = add_next prev_bb term_bb in
            process_bb scope decls term_bb cont_bb rest
       end
    | ReturnVoid pos :: rest ->
       let term_bb = make_terminal_bb ("ret_" ^ (label_of_pos pos)) None in
       let () = add_next prev_bb term_bb in
       process_bb scope decls term_bb cont_bb rest
    | TypeDecl _ :: _ ->
       Report.err_internal __FILE__ __LINE__
         "FIXME: TypeDecl not implemented Cfg.build_bbs (TypeDecl)"
    | Label (_, label) :: rest ->
       let bb = make_sequential_bb label [] in
       let () = add_next prev_bb bb in
       process_bb scope decls bb cont_bb rest
    | Goto (pos, label) :: rest ->
       let bb = make_goto_bb label in
       let () = add_next prev_bb bb in
       process_bb scope decls bb cont_bb rest
    | Continue pos :: rest ->
       begin match cont_bb with
       | None ->
          Report.err_internal __FILE__ __LINE__
            "Tried to continue without an enclosing loop scope."
       | Some condition ->
          let bb = make_sequential_bb ("cont_" ^ (label_of_pos pos)) [] in
          let () = add_next prev_bb bb in
          let () = add_next bb condition in
          process_bb scope decls bb cont_bb rest
       end
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
  let decls, bb = process_bb fcnscope [] entry_bb None body in
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
  List.rev decls, entry_bb

let verify_cfg name =
  let verify () = function
    | BB_Seq (label, _)
    | BB_Cond (label, _)
    | BB_Term (label, _) -> ()
    | BB_Goto (label, _) ->
       Report.err_internal __FILE__ __LINE__
         ("Found BB_Goto " ^ label ^ " in " ^ name)
    | BB_Error ->
       Report.err_internal __FILE__ __LINE__ ("Found BB_Error in " ^ name)
  in
  visit_df verify true ()

let build_fcns decltable typemap fcns = function
  | DefFcn (pos, _, name, _, body) ->
     let decls, entry_bb = build_bbs name decltable typemap body in
     let () = verify_cfg name entry_bb in
     let () = reset_bbs entry_bb in
     let fcn =
       { defn_begin = pos;
         defn_end = pos;
         name = name;
         local_vars = decls;
         entry_bb = entry_bb;
       }
     in fcn :: fcns
  | _ -> fcns

let builtin_types () =
  let map = make_symtab () in
  List.iter (fun (name, tp, _) ->
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
               let sz = size_of typemap (convert_type false typemap tp) in
               ExprLit (p, LitU32 (Int32.of_int sz))
            | _ :: [] -> Report.err_internal __FILE__ __LINE__
               "FIXME: No error message for this."
            | _ -> Report.err_internal __FILE__ __LINE__
               "FIXME: No error message for this."
            end
         | _ ->
            ExprFcnCall
              { fc_pos = f.fc_pos;
                fc_name = f.fc_name;
                fc_args = List.map process_expr f.fc_args
              }
       end
    | ExprBinary op ->
       let newop =
         { op_pos = op.op_pos;
           op_op = op.op_op;
           op_left = process_expr op.op_left;
           op_right = Some (process_expr (Util.the op.op_right))
         }
       in ExprBinary newop
    | ExprPreUnary op ->
       let newop =
         { op_pos = op.op_pos;
           op_op = op.op_op;
           op_left = process_expr op.op_left;
           op_right = None
         }
       in ExprPreUnary newop
    | ExprPostUnary op ->
       let newop =
         { op_pos = op.op_pos;
           op_op = op.op_op;
           op_left = process_expr op.op_left;
           op_right = None
         }
       in ExprPostUnary newop
    | ExprCast (p, v, e) ->
       ExprCast (p, v, process_expr e)
    | ExprIndex (p1, base, p2, idx) ->
       ExprIndex (p1, process_expr base, p2, process_expr idx)
    | ExprSelectField (p1, p2, e, fname) ->
       ExprSelectField (p1, p2, process_expr e, fname)
    | ExprStaticStruct (p, elist) ->
       ExprStaticStruct (p, List.map (fun (p, e) -> p, process_expr e) elist)
    | e -> e
  in
  let rec process_stmt = function
    | StmtExpr (p, e) -> StmtExpr (p, process_expr e)
    | Block (p, stmts) -> Block (p, List.map process_stmt stmts)
    | DefFcn (p, vis, name, tp, stmts) ->
       DefFcn(p, vis, name, tp, List.map process_stmt stmts)
    | VarDecl (vlist, tp) ->
       let fixedvlist = List.map (fun (p, nm, init) -> match init with
         | None -> (p, nm, init)
         | Some (p, e) -> (p, nm, Some (p, process_expr e)))
         vlist
       in
       VarDecl (fixedvlist, tp)
    | IfStmt (p, cond, tstmts, estmts_maybe) ->
       IfStmt (p, process_expr cond,
               List.map process_stmt tstmts,
               if estmts_maybe = None then None
               else Some (List.map process_stmt (Util.the estmts_maybe)))
    | WhileLoop (p, pre, cond, stmts) ->
       WhileLoop (p, pre, process_expr cond, List.map process_stmt stmts)
    | Return (p, e) -> Return (p, process_expr e)
    | stmt -> stmt
  in
  List.map process_stmt stmts

let convert_ast stmts =
  let typemap = builtin_types () in
  let decltable = make_symtab () in
  List.iter (global_types typemap) stmts;
  let typemap = symtab_filter (resolve_type typemap) typemap in
  let sanitized_stmts = resolve_builtins stmts typemap in
  List.iter (global_decls decltable typemap) sanitized_stmts;
  let fcnlist =
    List.fold_left (build_fcns decltable typemap) [] sanitized_stmts
  in
  { global_decls = decltable;
    fcnlist = List.rev fcnlist;
    deftypemap = typemap
  }
