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
open Iropt
open Llvm
open Llvmext (* build_cmpxchg *)
open Types
open Util

type llvm_data =
  { ctx  : llcontext;
    mdl  : llmodule;
    bldr : llbuilder;
    typemap : lltype symtab;
    prog : program;
    zero_i32 : llvalue;
    one_i8 : llvalue;
    one_i16 : llvalue;
    one_i32 : llvalue;
    one_i64 : llvalue;
    one_f32 : llvalue;
    one_f64 : llvalue
  }

let get_fcntype = function
  | DefTypeFcn (params, ret, variadic) -> params, ret, variadic
  | _ -> Report.err_internal __FILE__ __LINE__ "Expected a DefTypeFcn."

let deftype2llvmtype ctx typemap =
  let rec convert wrap_fcn_ptr = function
    | DefTypeUnresolved (_, name) ->
       Report.err_internal __FILE__ __LINE__
         ("Tried to convert a placeholder type: " ^ name)
    | DefTypeVoid ->
       the (lookup_symbol typemap "void")
    | DefTypeFcn (args, ret, variadic) ->
       let llvmargs = List.map (fun argtp -> convert true argtp) args in
       let fbuild = if variadic then var_arg_function_type else function_type
       in
       let ftype = fbuild (convert true ret) (Array.of_list llvmargs) in
       if wrap_fcn_ptr then pointer_type ftype
       else ftype
    | DefTypePrimitive (prim, _) ->
       let name = primitive2string prim in
       the (lookup_symbol typemap name)
    | DefTypeArray (tp, dim) ->
       array_type (convert wrap_fcn_ptr tp) dim
    | DefTypePtr DefTypeVoid ->
       (* Void pointer isn't natively supported by LLVM.  Use char* instead. *)
       pointer_type (the (lookup_symbol typemap "i8"))
    | DefTypePtr pointed_to_tp ->
       pointer_type (convert wrap_fcn_ptr pointed_to_tp)
    | DefTypeNamedStruct name ->
       the (lookup_symbol typemap name)
    | DefTypeStaticStruct members
    | DefTypeLiteralStruct (members, _) ->
       let llvm_members = List.map (convert wrap_fcn_ptr) members in
       struct_type ctx (Array.of_list llvm_members)
    | _ ->
       Report.err_internal __FILE__ __LINE__ "deftype2llvmtype"
  in convert

let build_types ctx deftypes =
  let typemap = make_symtab () in
  let forward_declare_structs name deftype =
    match lookup_symbol typemap name with
    | Some _ -> ()
    | None ->
       begin match deftype with
       | DefTypeLiteralStruct _ ->
          add_symbol typemap name (named_struct_type ctx name)
       | DefTypePrimitive _
       | DefTypePtr _ ->
          add_symbol typemap name (deftype2llvmtype ctx typemap true deftype)
       | _ -> Report.err_internal __FILE__ __LINE__
          "Some type other than named struct was not found."
       end
  in
  let build_structs name = function
    | DefTypeLiteralStruct (members, _) ->
       let llvm_members =
         List.map (deftype2llvmtype ctx typemap true) members in
       let llvm_struct = the (lookup_symbol typemap name) in
       struct_set_body llvm_struct (Array.of_list llvm_members) false
    | _ -> ()
  in
  List.iter
    (fun (name, _, f, _) -> add_symbol typemap name (f ctx))
    Types.map_builtin_types;
  symtab_iter forward_declare_structs deftypes;
  symtab_iter build_structs deftypes;
  typemap

let process_literal typemap lit = match lit with
  | LitBool true ->
     const_int (the (lookup_symbol typemap "bool")) 1
  | LitBool false ->
     const_int (the (lookup_symbol typemap "bool")) 0
  | LitI8 n
  | LitU8 n ->
     const_int (the (lookup_symbol typemap "i8")) (Char.code n)
  | LitI16 n
  | LitU16 n
  | LitI32 n
  | LitU32 n ->
     let typename = primitive2string (literal2primitive lit) in
     const_int (the (lookup_symbol typemap typename)) (Int32.to_int n)
  | LitI64 n ->
     const_of_int64 (the (lookup_symbol typemap "i64")) n true
  | LitU64 n ->
     const_of_int64 (the (lookup_symbol typemap "i64")) n false
  | LitF32 n ->
     const_float (the (lookup_symbol typemap "f32")) n
  | LitF64 n ->
     const_float (the (lookup_symbol typemap "f64")) n

let bytes = function
  | DefTypePrimitive (p, _) ->
     begin match p with
     | PrimBool | PrimI8 | PrimU8 -> 1
     | PrimI16 | PrimU16 -> 2
     | PrimI32 | PrimU32 -> 4
     | PrimI64 | PrimU64 -> 8
     | PrimF32 -> 4
     | PrimF64 -> 8
     end
  | _ ->
     Report.err_internal __FILE__ __LINE__ "Non primitive type."

(* FIXME: WORKING HERE:  Need to mark loads/stores volatile -- not types. *)
let process_expr data varmap pos_n_expr =
  let rec llvm_unop op tp expr pre_p bldr =
    match op with
    | OperIncr ->
       let addr = expr_gen false expr in
       let value = build_load addr "deref" bldr in
       let incr_result = match is_integer_type tp, bytes tp with
         | true, 1 -> build_add value data.one_i8 "incr" bldr
         | true, 2 -> build_add value data.one_i16 "incr" bldr
         | true, 4 -> build_add value data.one_i32 "incr" bldr
         | true, 8 -> build_add value data.one_i64 "incr" bldr
         | false, 4 -> build_fadd value data.one_f32 "fincr" bldr
         | false, 8 -> build_fadd value data.one_f64 "fincr" bldr
         | _ ->
            Report.err_internal __FILE__ __LINE__
                                "Tried to increment unknown type."
       in
       let _ = build_store incr_result addr bldr in
       if pre_p then incr_result else value
    | OperAddrOf ->
       let llvm_expr = expr_gen false expr in
       build_gep llvm_expr [| data.zero_i32 |] "addrof" bldr
    | OperMinus ->
       let llvm_expr = expr_gen true expr in
       (if is_sinteger_type tp then build_nsw_neg
        else if is_uinteger_type tp then build_neg
        else build_fneg)
         llvm_expr "neg" bldr
    | OperLogicalNot ->
       let llvm_expr = expr_gen true expr in
       build_not llvm_expr "lnot" bldr
    | _ ->
       Report.err_internal __FILE__ __LINE__
         ("llvm_unop not fully implemented: operator " ^ (operator2string op))
  and llvm_binop op tp left right bldr =
    let standard_op fnc name =
      fnc (expr_gen true left) (expr_gen true right) name bldr
    in
    match op, (is_integer_type tp) || (is_pointer_type tp) with
    (* FIXME: Distinguish between signed/unsigned integers. *)
    | OperMult, true -> standard_op build_mul "def_mult"
    | OperMult, false -> standard_op build_fmul "def_mult_f"
    | OperDiv, true -> standard_op build_sdiv "def_sdiv"
    | OperDiv, false -> standard_op build_fdiv "def_div_f"
    | OperRemainder, true ->
       let op = if signed_p tp then build_srem else build_urem in
       standard_op op "def_rem"
    | OperLShift, true -> standard_op build_shl "def_shl"
    | OperPlus, true -> standard_op build_add "def_add"
    | OperPlus, false -> standard_op build_fadd "def_add_f"
    | OperMinus, true -> standard_op build_sub "def_sub"
    | OperMinus, false -> standard_op build_fsub "def_sub_f"
    | OperLT, true -> standard_op (build_icmp Icmp.Slt) "def_lt"
    | OperLT, false -> standard_op (build_fcmp Fcmp.Olt) "def_lt_f"
    | OperLTE, true -> standard_op (build_icmp Icmp.Sle) "def_le"
    | OperLTE, false -> standard_op (build_fcmp Fcmp.Ole) "def_le_f"
    | OperGT, true -> standard_op (build_icmp Icmp.Sgt) "def_gt"
    | OperGT, false -> standard_op (build_fcmp Fcmp.Ogt) "def_gt_f"
    | OperGTE, true -> standard_op (build_icmp Icmp.Sge) "def_ge"
    | OperGTE, false -> standard_op (build_fcmp Fcmp.Oge) "def_ge_f"
    | OperEquals, true -> standard_op (build_icmp Icmp.Eq) "def_eq"
    | OperEquals, false -> standard_op (build_fcmp Fcmp.Oeq) "def_eq_f"
    | OperNEquals, true -> standard_op (build_icmp Icmp.Ne) "def_neq"
    | OperNEquals, false -> standard_op (build_fcmp Fcmp.One) "def_neq_f"
    | OperBitwiseAnd, true -> standard_op build_and "def_band"
    | OperBitwiseOr, true -> standard_op build_or "def_bor"
    | OperLogicalAnd, true -> standard_op build_and "def_land" (* FIXME!!! *)
    | OperAssign, _ ->
       begin match left, right with
       | _, Expr_Cast (_, _, Expr_StaticStruct (_, members))
       | _, Expr_StaticStruct (_, members) ->
          let base = expr_gen false left in
          let assign n (_, expr) =
            let v = expr_gen true expr in
            let dest = build_struct_gep base n "ssfield" bldr in
            ignore (build_store v dest bldr)
          in
          let () = List.iteri assign members in
          base (* FIXME: Broken!  BAD WILLTOR!  NO BISCUIT!  But I don't want
                  to fix this, now.
                  This returns an address rather than a proper rvalue.  It
                  needs a dereference, somehow, but without recalculating. *)
       | _ ->
          let rhs = expr_gen true right in
          let _ = build_store rhs (expr_gen false left) bldr in
          rhs
       end
    | OperBitwiseAnd, false
    | OperBitwiseOr, false ->
       Report.err_internal __FILE__ __LINE__
         ("tried to perform an operation \"" ^ (operator2string op)
          ^ ") on float operands.  This should have been caught earlier.")
    | _ ->
       Report.err_internal __FILE__ __LINE__
         ("llvm_operator not fully implemented: operator "
          ^ (operator2string op))

  and make_llvm_tp = function
    | DefTypePtr t ->
       pointer_type (make_llvm_tp t)
    | DefTypePrimitive (pt, _) ->
       the (lookup_symbol data.typemap (primitive2string pt))
    | DefTypeNamedStruct nm ->
       the (lookup_symbol data.typemap nm)
    | DefTypeVoid ->
       the (lookup_symbol data.typemap "i8")
    | t -> Report.err_internal __FILE__ __LINE__
       ("make_llvm_tp incomplete: " ^ (string_of_type t))

  and build_cast from_tp to_tp e =
    let build_primitive_cast t1 t2 =
      let llvm_to_tp =
        (the (lookup_symbol data.typemap (primitive2string t2))) in
      let null_cast _ _ _ _ = e in
      let f = match t1, t2 with

        (* From bool *)
        | PrimBool, PrimI8  | PrimBool, PrimU8
        | PrimBool, PrimI16 | PrimBool, PrimU16
        | PrimBool, PrimI32 | PrimBool, PrimU32
        | PrimBool, PrimI64 | PrimBool, PrimU64 -> build_zext
        | PrimBool, PrimF32
        | PrimBool, PrimF64 -> build_uitofp

        (* From i8 *)
        | PrimI8, PrimBool -> build_trunc
        | PrimI8, PrimU8 -> null_cast
        | PrimI8, PrimI16 | PrimI8, PrimI32 | PrimI8, PrimI64 -> build_sext
        | PrimI8, PrimU16 | PrimI8, PrimU32 | PrimI8, PrimU64 -> build_zext
        | PrimI8, PrimF32
        | PrimI8, PrimF64 -> build_sitofp

        (* From u8 *)
        | PrimU8, PrimBool -> build_trunc
        | PrimU8, PrimI8 -> null_cast
        | PrimU8, PrimI16 | PrimU8, PrimI32 | PrimU8, PrimI64
        | PrimU8, PrimU16 | PrimU8, PrimU32 | PrimU8, PrimU64 -> build_zext
        | PrimU8, PrimF32
        | PrimU8, PrimF64 -> build_sitofp

        (* From i16 *)
        | PrimI16, PrimBool
        | PrimI16, PrimU8  | PrimI16, PrimI8
          -> build_trunc
        | PrimI16, PrimU16 -> null_cast
        | PrimI16, PrimI32 | PrimI16, PrimI64 -> build_sext
        | PrimI16, PrimU32 | PrimI16, PrimU64 -> build_zext
        | PrimI16, PrimF32
        | PrimI16, PrimF64 -> build_sitofp

        (* From u16 *)
        | PrimU16, PrimBool
        | PrimU16, PrimU8  | PrimU16, PrimI8
          -> build_trunc
        | PrimU16, PrimI16 -> null_cast
        | PrimU16, PrimI32 | PrimU16, PrimU32
        | PrimU16, PrimI64 | PrimU16, PrimU64 -> build_zext
        | PrimU16, PrimF32
        | PrimU16, PrimF64 -> build_uitofp

        (* From i32 *)
        | PrimI32, PrimBool
        | PrimI32, PrimU8  | PrimI32, PrimI8
        | PrimI32, PrimI16 | PrimI32, PrimU16 -> build_trunc
        | PrimI32, PrimU32 -> null_cast
        | PrimI32, PrimI64 -> build_sext
        | PrimI32, PrimU64 -> build_zext
        | PrimI32, PrimF32
        | PrimI32, PrimF64 -> build_sitofp

        (* From u32 *)
        | PrimU32, PrimBool
        | PrimU32, PrimU8  | PrimU32, PrimI8
        | PrimU32, PrimI16 | PrimU32, PrimU16 -> build_trunc
        | PrimU32, PrimI32 -> null_cast
        | PrimU32, PrimI64 | PrimU32, PrimU64 -> build_zext
        | PrimU32, PrimF32
        | PrimU32, PrimF64 -> build_uitofp

        (* From i64 *)
        | PrimI64, PrimBool
        | PrimI64, PrimU8  | PrimI64, PrimI8
        | PrimI64, PrimI16 | PrimI64, PrimU16
        | PrimI64, PrimI32 | PrimI64, PrimU32 -> build_trunc
        | PrimI64, PrimU64 -> null_cast
        | PrimI64, PrimF32
        | PrimI64, PrimF64 -> build_sitofp

        (* From u64 *)
        | PrimU64, PrimBool
        | PrimU64, PrimU8  | PrimU64, PrimI8
        | PrimU64, PrimI16 | PrimU64, PrimU16
        | PrimU64, PrimI32 | PrimU64, PrimU32 -> build_trunc
        | PrimU64, PrimI64 -> null_cast
        | PrimU64, PrimF32
        | PrimU64, PrimF64 -> build_uitofp

        (* From f32 *)
        | PrimF32, PrimBool
        | PrimF32, PrimI8 | PrimF32, PrimI16 | PrimF32, PrimI32
        | PrimF32, PrimI64
          -> build_fptosi
        | PrimF32, PrimU8 | PrimF32, PrimU16 | PrimF32, PrimU32
        | PrimF32, PrimU64
          -> build_fptoui
        | PrimF32, PrimF64 -> build_fpext

        (* From f64 *)
        | PrimF64, PrimBool
        | PrimF64, PrimI8 | PrimF64, PrimI16 | PrimF64, PrimI32
        | PrimF64, PrimI64
          -> build_fptosi
        | PrimF64, PrimU8 | PrimF64, PrimU16 | PrimF64, PrimU32
        | PrimF64, PrimU64
          -> build_fptoui
        | PrimF64, PrimF32 -> build_fptrunc

        | _ -> Report.err_internal __FILE__ __LINE__
           ("Cast from " ^ (primitive2string t1) ^ " to "
            ^ (primitive2string t2))
      in
      f e llvm_to_tp "cast" data.bldr
    in
    match from_tp, to_tp with
    | DefTypePrimitive (prim1, _), DefTypePrimitive (prim2, _) ->
       (* FIXME: qualifiers. *)
       build_primitive_cast prim1 prim2
    | DefTypePtr _, DefTypePtr _ ->
       build_bitcast e (make_llvm_tp to_tp) "cast" data.bldr
    | DefTypePtr _, DefTypePrimitive _ ->
       if is_integer_type to_tp then
         build_pointercast e (make_llvm_tp to_tp) "cast" data.bldr
       else Report.err_internal __FILE__ __LINE__
         "What's the deal with casting pointers to floats?"
    | DefTypePrimitive _, DefTypePtr _ ->
       if is_integer_type from_tp then
         build_inttoptr e (make_llvm_tp to_tp) "cast" data.bldr
       else Report.err_internal __FILE__ __LINE__
         "What's the deal with casting floats to ptrs?"
    | DefTypeStaticStruct _, DefTypeNamedStruct s
    | DefTypeLiteralStruct _, DefTypeNamedStruct s ->
       build_bitcast e (make_llvm_tp (DefTypeNamedStruct s)) "scast" data.bldr
    | DefTypeStaticStruct _, _
    | DefTypeLiteralStruct _, _ ->
       (* FIXME: I think this might be correct, actually, but need to think
          about it some more. *)
       e
    | DefTypeArray (tp, n), DefTypePtr _ ->
       build_in_bounds_gep e [|data.zero_i32; data.zero_i32|]
                           "ptr_cast" data.bldr
    | _ ->
       Report.err_internal __FILE__ __LINE__
         ("build_cast: Incomplete implementation (from "
          ^ (string_of_type from_tp) ^ " to "
          ^ (string_of_type to_tp) ^ ").")

  and expr_gen rvalue_p = function
    | Expr_New (tp, sz, init) ->
       let llsz = expr_gen true sz in
       let _, _, alloc = the (lookup_symbol varmap "forkscan_malloc") in
       let mem = build_call alloc [| llsz |] "new" data.bldr in
       let obj = build_cast (DefTypePtr DefTypeVoid) (DefTypePtr tp) mem in
       let () = List.iter (fun (n, e) ->
                    let addr = build_struct_gep obj n "member" data.bldr in
                    let v = expr_gen true e in
                    ignore(build_store v addr data.bldr))
                          init
       in
       obj
    | Expr_FcnCall (name, args) ->
       let (_, tp, var) = the (lookup_symbol varmap name) in
       let arg_vals = List.map (expr_gen true) args in
       (* Protect programmers from this function pointer nonsense. *)
       let callee = match classify_value var with
         | ValueKind.Function -> var
         | _ -> build_load var "callee" data.bldr
       in
       let retname = match tp with
         | DefTypeFcn (_, DefTypeVoid, _) -> ""
         | _ -> "def_call"
       in
       build_call callee (Array.of_list arg_vals) retname data.bldr
    | Expr_String (label, value) ->
       let str = const_stringz data.ctx value in
       let ptr = define_global label str data.mdl in
       (* FIXME: what about rvalue? *)
       ptr
    | Expr_Literal lit -> process_literal data.typemap lit
    | Expr_Variable name ->
       begin match lookup_symbol varmap name with
       | None -> Report.err_internal __FILE__ __LINE__
          ("Failed to find variable " ^ name ^ ".")
       | Some (_, DefTypeFcn _, llvar) ->
          if not rvalue_p then llvar
          else begin match classify_value llvar with
          | ValueKind.Function -> llvar
          | _ -> build_load llvar "deref_fcn" data.bldr
          end
       | Some (_, DefTypeArray _, llvar) ->
          (* Arrays need to be treated differently from pointers - they
             should not be dereferenced here.  The array llvar _is_ the
             pointer to the memory, not a pointer to the pointer. *)
          llvar
       | Some (_, _, llvar) ->
          if rvalue_p then build_load llvar name data.bldr
          else llvar
       end
    | Expr_Binary (op, tp, left, right) ->
       llvm_binop op tp left right data.bldr
    | Expr_Unary (op, tp, expr, pre_p) ->
       llvm_unop op tp expr pre_p data.bldr
    | Expr_Cast (_, to_tp, Expr_Nil) ->
       const_null (make_llvm_tp to_tp)
    | Expr_Cast (from_tp, to_tp, expr) ->
       let e = expr_gen true expr in
       build_cast from_tp to_tp e
    | Expr_Index (base, idx, deref_base, array) ->
       let i = expr_gen true idx in
       let b = expr_gen deref_base base in
       let addr =
         if not array then build_gep b [|i|] "addr" data.bldr
         else build_in_bounds_gep b [|data.zero_i32; i|] "addr" data.bldr
       in
       if rvalue_p then build_load addr "deref" data.bldr
       else addr
    | Expr_SelectField (expr, n, is_volatile) ->
       let base = expr_gen false expr in
       let addr = build_struct_gep base n "maddr" data.bldr in
       if rvalue_p then
         let instr = build_load addr "mval" data.bldr in
         let () = set_volatile true instr in
         instr
       else addr
    | Expr_StaticStruct (None, members) ->
       let _, elements = List.split members in
       const_struct data.ctx
         (Array.of_list (List.map (expr_gen true) elements))
    | Expr_StaticStruct (Some nm, members) ->
       let _, elements = List.split members in
       const_named_struct (the (lookup_symbol data.typemap nm))
         (Array.of_list (List.map (expr_gen true) elements))
    | Expr_Nil ->
       Report.err_internal __FILE__ __LINE__ "Nil."
    | Expr_Atomic (AtomicCAS, [(_, dexpr);
                               (_, cexpr);
                               (_, vexpr)]) ->
       let dest = expr_gen false dexpr in
       let cmp = expr_gen true cexpr in
       let v = expr_gen true vexpr in
       let cmpxchg = build_cmpxchg dest cmp v
         AtomicOrdering.SequentiallyConsistent
         AtomicOrdering.SequentiallyConsistent
         "cmpxchg"
         data.bldr in
       build_extractvalue cmpxchg 1 "cmpxchg_succ" data.bldr
    | Expr_Atomic (AtomicCAS, _) ->
       Report.err_internal __FILE__ __LINE__ "CAS on != 3 parameters"
    | Expr_Atomic (AtomicSwap, [(_, dexpr);
                                (_, vexpr)]) ->
       let dest = expr_gen false dexpr in
       let v = expr_gen true vexpr in
       build_atomicrmw AtomicRMWBinOp.Xchg dest v
                       AtomicOrdering.Acquire
                       false
                       "swap"
                       data.bldr
    | Expr_Atomic (AtomicSwap, _) ->
       Report.err_internal __FILE__ __LINE__ "Swap on != 2 parameters"
  in
  let _, expr = pos_n_expr in
  expr_gen true expr

let process_fcn cgdebug data symbols fcn =
  let profile = the (lookup_symbol data.prog.global_decls fcn.name) in
  let (_, _, llfcn) = the (lookup_symbol symbols profile.mappedname) in
  let llblocks = Hashtbl.create 32 in
  let make_llbbs () = function
    | BB_Seq (label, _)
    | BB_Cond (label, _)
    | BB_Term (label, _) ->
       Hashtbl.add llblocks label (append_block data.ctx label llfcn)
    | _ -> Report.err_internal __FILE__ __LINE__ "Unexpected block type."
  in

  let varmap = push_symtab_scope symbols in
  let () = Cfg.reset_bbs fcn.entry_bb in
  let () = Cfg.visit_df make_llbbs true () fcn.entry_bb in

  let get_bb = Hashtbl.find llblocks in

  let get_label = function
    | BB_Seq (label, _)
    | BB_Cond (label, _)
    | BB_Term (label, _) -> label
    | _ -> Report.err_internal __FILE__ __LINE__ "Unexpected block type."
  in

  let populate_blocks () = function
    | BB_Seq (label, block) ->
       let bb = get_bb label in
       let () = position_at_end bb data.bldr in
       let () = List.iter
         (fun expr -> ignore (process_expr data varmap expr)) block.seq_expr in
       let next_bb = get_bb (get_label block.seq_next) in
       let _ = build_br next_bb data.bldr in
       ()
    | BB_Cond (label, block) ->
       let bb = get_bb label in
       let () = position_at_end bb data.bldr in
       let cond = process_expr data varmap block.cond_branch
       and then_bb = get_bb (get_label block.cond_next)
       and else_bb = get_bb (get_label block.cond_else) in
       let _ = build_cond_br cond then_bb else_bb data.bldr in
       ()
    | BB_Term (label, block) ->
       let bb = get_bb label in
       let () = position_at_end bb data.bldr in
       begin match block.term_expr with
       | None -> let _ = build_ret_void data.bldr in ()
       | Some e ->
          let _ = build_ret (process_expr data varmap e) data.bldr in ()
       end
    | _ -> Report.err_internal __FILE__ __LINE__ "Unexpected block type."
  in

  let entry = get_bb "entry" in
  position_at_end entry data.bldr;
  let (args, _, _) = get_fcntype profile.tp in
  let llparams = params llfcn in
  List.iteri (fun i ((pos, n), tp) ->
    let alloc =
      build_alloca (deftype2llvmtype data.ctx data.typemap true tp) n data.bldr
    in begin
      add_symbol varmap n (pos, tp, alloc);
      ignore (build_store llparams.(i) alloc data.bldr);
    end) (List.combine profile.params args);
  List.iter (fun (name, decl) ->
    let alloc = build_alloca
      (deftype2llvmtype data.ctx data.typemap true decl.tp) name data.bldr
    in add_symbol varmap name (decl.decl_pos, decl.tp, alloc))
    fcn.local_vars;

  let () = Cfg.visit_df populate_blocks false () fcn.entry_bb in

  if not cgdebug then Llvm_analysis.assert_valid_function llfcn

let declare_globals data symbols name decl =
  let llfcn =
    (* FIXME: Might not be a function.  Need to check decl.tp. *)
    declare_function
      decl.mappedname (deftype2llvmtype data.ctx data.typemap false decl.tp)
      data.mdl
  in
  if decl.vis = VisLocal then set_linkage Linkage.Internal llfcn;
  add_symbol symbols decl.mappedname (decl.decl_pos, decl.tp, llfcn)

let process_cfg cgdebug module_name program opt_level =
  let ctx  = global_context () in
  let mdl  = create_module ctx module_name in
  let bldr = builder ctx in
  let pm = create_pm opt_level in
  let typemap = build_types ctx program.deftypemap in
  let data = { ctx = ctx;
               mdl = mdl;
               bldr = bldr;
               typemap = typemap;
               prog = program;
               zero_i32 = const_null (the (lookup_symbol typemap "i32"));
               one_i8 = const_int (the (lookup_symbol typemap "bool")) 1;
               one_i16 = const_int (the (lookup_symbol typemap "i16")) 1;
               one_i32 = const_int (the (lookup_symbol typemap "i32")) 1;
               one_i64 = const_int (the (lookup_symbol typemap "i64")) 1;
               one_f32 = const_float (the (lookup_symbol typemap "f32")) 1.0;
               one_f64 = const_float (the (lookup_symbol typemap "f64")) 1.0
             } in
  let symbols = make_symtab () in
  symtab_iter (declare_globals data symbols) program.global_decls;
  List.iter (process_fcn cgdebug data symbols) program.fcnlist;
  ignore (PassManager.run_module mdl pm);
  mdl
