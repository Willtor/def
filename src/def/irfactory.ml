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
open Lexing
open Llvm
open Llvmext (* build_cmpxchg, token_type *)
open Llvm_target
open Operator
open Types
open Util

type llvm_data =
  { ctx      : llcontext;
    mdl      : llmodule;
    bldr     : llbuilder;
    typemap  : lltype symtab;
    prog     : program;
    fattrs   : llattribute list;

    (* Debug info. *)
    mutable difile : llvalue option;
    mutable dib    : lldibuilder option;
    d_scope_table  : (cfg_scope, cfg_scope) Hashtbl.t;

    (* Constant values. *)
    zero_i32 : llvalue;
    one_i8   : llvalue;
    one_i16  : llvalue;
    one_i32  : llvalue;
    one_i64  : llvalue;
    one_f32  : llvalue;
    one_f64  : llvalue
  }

let va_list_tag = "struct.__va_list_tag"

let fcn_attrs =
  [("target-features", "+fxsr,+mmx,+rtm,+sse,+sse2,+x87");
   ("target-cpu", "x86-64")
  ]

let debug_locations = Hashtbl.create 32

let get_fcntype = function
  | DefTypeFcn (params, ret, variadic) -> params, ret, variadic
  | _ -> Report.err_internal __FILE__ __LINE__ "Expected a DefTypeFcn."

let convert_deftype2llvmtype ctx typemap deftypemap =
  let rec convert wrap_fcn_ptr tp =
    match tp.bare with
    | DefTypeUnresolved name ->
       Report.err_internal __FILE__ __LINE__
         ("Tried to convert a placeholder type: " ^ name)
    | DefTypeVoid ->
       the (lookup_symbol typemap "void")
    | DefTypeNamed nm ->
       let ret = match lookup_symbol typemap nm with
         | Some t -> t
         | None ->
            begin match lookup_symbol deftypemap nm with
              | Some t ->
                 Report.err_internal __FILE__ __LINE__
                                     (nm ^ " in one map but not the other.")
              | None ->
                 Report.err_internal __FILE__ __LINE__
                                     (nm ^ " in neither map.")
            end
       in
       ret
    | DefTypeOpaque nm ->
       let () = prerr_endline ("shouldn't lookup an opaque type " ^ nm) in
       Util.the @@ lookup_symbol typemap nm
    | DefTypeFcn (args, ret, variadic) ->
       let llvmargs = List.map (fun argtp -> convert true argtp) args in
       let fbuild = if variadic then var_arg_function_type else function_type
       in
       let ftype = fbuild (convert true ret) (Array.of_list llvmargs) in
       if wrap_fcn_ptr then pointer_type ftype
       else ftype
    | DefTypePrimitive prim ->
       let name = primitive2string prim in
       the (lookup_symbol typemap name)
    | DefTypePtr ({ bare = DefTypeVoid }) ->
       (* Void pointer isn't natively supported by LLVM.  Use char* instead. *)
       pointer_type (the (lookup_symbol typemap "i8"))
    | DefTypePtr ({ bare = DefTypeOpaque _ }) ->
       (* Opaque struct pointers are just i8 pointers, as far as we're
          concerned.  As long as an opaque type remains opaque, it doesn't
          matter. *)
       pointer_type (the (lookup_symbol typemap "i8"))
    | DefTypePtr pointed_to_tp ->
       pointer_type (convert wrap_fcn_ptr pointed_to_tp)
    | DefTypeArray (tp, dim) ->
       array_type (convert wrap_fcn_ptr tp) dim
    | DefTypeNullPtr ->
       Report.err_internal __FILE__ __LINE__ "convert null pointer."
    | DefTypeEnum _ ->
       let sz = size_of deftypemap tp in
       integer_type ctx (sz * 8)
    | DefTypeStaticStruct members
    | DefTypeLiteralStruct (members, _) ->
       let llvm_members = List.map (convert wrap_fcn_ptr) members in
       struct_type ctx (Array.of_list llvm_members)
    | DefTypeLiteralUnion _ ->
       (* FIXME: This should take alignment into account.  There're going to
          be troubles if we don't. *)
       let sz = Types.size_of deftypemap tp in
       let array = array_type (the @@ lookup_symbol typemap "i8") sz
       in struct_type ctx [| array |]
    | DefTypeVAList ->
       Util.the @@ lookup_symbol typemap va_list_tag
    | DefTypeLLVMToken -> token_type ctx
    | DefTypeWildcard ->
       Report.err_internal __FILE__ __LINE__ "convert wildcard."
  in convert

let deftype2llvmtype data =
  convert_deftype2llvmtype data.ctx data.typemap data.prog.deftypemap

let build_types ctx deftypes =
  let typemap = make_symtab () in
  let do_convert = convert_deftype2llvmtype ctx typemap deftypes true in
  let forward_declare_structs name deftype =
    match lookup_symbol typemap name with
    | Some _ -> ()
    | None ->
       begin match (concrete_of deftypes deftype).bare with
       | DefTypeLiteralStruct _ ->
          add_symbol typemap name (named_struct_type ctx name)
       | DefTypeLiteralUnion _ ->
          add_symbol typemap name (named_struct_type ctx name)
       | DefTypeOpaque _ ->
          add_symbol typemap name (named_struct_type ctx name)
       | _ -> ()
       end
  in
  let build_primitives name deftype =
    match lookup_symbol typemap name with
    | Some _ -> ()
    | None ->
       begin match (concrete_of deftypes deftype).bare with
       | DefTypePrimitive _ ->
          add_symbol typemap name (do_convert deftype)
       | DefTypeEnum _ ->
          let sz = size_of deftypes deftype in
          let tp = integer_type ctx (sz * 8) in
          add_symbol typemap name tp
       | DefTypeVAList ->
          (* FIXME: Platform-specific struct: struct.va_list. *)
          let t name = Util.the @@ lookup_symbol typemap name in
          let valist = named_struct_type ctx va_list_tag in
          let () = struct_set_body valist
                                   [| t "i32";
                                      t "i32";
                                      pointer_type @@ t "i8";
                                      pointer_type @@ t "i8"
                                   |]
                                   false
          in
          let () = add_symbol typemap va_list_tag valist in
          add_symbol typemap name valist
       | _ -> ()
       end
  in
  let build_non_structs name deftype =
    match lookup_symbol typemap name with
    | Some _ -> ()
    | None ->
       begin match (concrete_of deftypes deftype).bare with
       | DefTypeLiteralStruct _
       | DefTypeLiteralUnion _
       | DefTypeOpaque _
       | DefTypePrimitive _
       | DefTypeEnum _
       | DefTypeVAList ->
          ()
       | DefTypePtr _ ->
          add_symbol typemap name (do_convert deftype)
       | DefTypeNamed nm ->
          Report.err_internal __FILE__ __LINE__ ("named type: " ^ nm)
       | DefTypeFcn (params, ret, is_variadic) ->
          let llparams = List.map do_convert params in
          let llret = do_convert ret in
          let lltype =
            (if is_variadic then var_arg_function_type
             else function_type)
              llret (Array.of_list llparams)
          in
          add_symbol typemap name lltype
       | DefTypeVoid ->
          (* Sometimes comes out of a typedef.  Any typing problems should have
             been caught earlier in compilation, so it's okay to simpy i8 the
             thing. *)
          add_symbol typemap name (Util.the @@ lookup_symbol typemap "i8")

       (* Shouldn't be seeing any of the following types at this stage in the
          compilation. *)
       | DefTypeNullPtr ->
          Report.err_internal __FILE__ __LINE__ "null pointer type."
       | DefTypeArray (atype, sz) ->
          let elements = do_convert atype in
          add_symbol typemap name (array_type elements sz)
       | DefTypeUnresolved s ->
          Report.err_internal __FILE__ __LINE__
                              ("unresolved type " ^ s ^ " from: "
                               ^ (Error.format_position
                                    (Util.the deftype.dtpos)))
       | _ ->
          Report.err_internal __FILE__ __LINE__
                              "Some unknown type was found."
       end
  in
  let build_structs name tp =
    match tp.bare with
    | DefTypeLiteralStruct (members, _) ->
       let llvm_members =
         List.map do_convert members in
       let llvm_struct = the (lookup_symbol typemap name) in
       struct_set_body llvm_struct (Array.of_list llvm_members) false
    | DefTypeLiteralUnion _ ->
       let sz = Types.size_of deftypes tp in
       let array = array_type (the @@ lookup_symbol typemap "i8") sz in
       let llvm_struct = the (lookup_symbol typemap name) in
       struct_set_body llvm_struct [| array |] false
    | _ -> ()
  in
  List.iter
    (fun (name, _, f, _, _, _) -> add_symbol typemap name (f ctx))
    Types.map_builtin_types;
  symtab_iter forward_declare_structs deftypes;
  symtab_iter build_primitives deftypes;
  symtab_iter build_non_structs deftypes;
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

let bytes tp =
  match tp.bare with
  | DefTypePrimitive p ->
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

let get_debug_location data =
  let dib = Util.the data.dib in
  let parent_scope scope =
    try Hashtbl.find data.d_scope_table scope
    with _ -> Report.err_internal __FILE__ __LINE__ "incomplete debug info."
  in
  let rec get loc =
    try Hashtbl.find debug_locations loc
    with _ ->
      let file = Util.the data.difile in
      match loc with
      | ScopeGlobal pos ->
         let dilex = dilexical_block data.ctx dib pos file file in
         begin
           Hashtbl.add debug_locations loc dilex;
           dilex
         end
      | ScopeLexical pos ->
         let pscope = parent_scope loc in
         let llpscope = get pscope in
         let dilex = dilexical_block data.ctx dib pos llpscope file in
         begin
           Hashtbl.add debug_locations loc dilex;
           dilex
         end
      | ScopeLeaf pos ->
         let pscope = parent_scope loc in
         let llpscope = get pscope in
         let diloc = dilocation data.ctx pos llpscope in
         begin
           Hashtbl.add debug_locations loc diloc;
           diloc
         end
  in
  get

let process_expr data llvals varmap pos_n_expr =
  let i32type = the (lookup_symbol data.typemap "i32") in
  let constval = const_int i32type in
  let build_load_wrapper addr tp name bldr =
    let deref = build_load addr name bldr in
    if dt_is_volatile tp then set_volatile true deref;
    deref
  in
  let make_debug_location pos = get_debug_location data (ScopeLeaf pos) in
  let rec llvm_unop op tp expr pre_p bldr =
    match op with
    | OperIncr ->
       let addr = expr_gen false expr in
       let value = build_load_wrapper addr tp "deref" bldr in
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
    | OperDecr ->
       let addr = expr_gen false expr in
       let value = build_load_wrapper addr tp "deref" bldr in
       let decr_result = match is_integer_type tp, bytes tp with
         | true, 1 -> build_sub value data.one_i8 "decr" bldr
         | true, 2 -> build_sub value data.one_i16 "decr" bldr
         | true, 4 -> build_sub value data.one_i32 "decr" bldr
         | true, 8 -> build_sub value data.one_i64 "decr" bldr
         | false, 4 -> build_fsub value data.one_f32 "fdecr" bldr
         | false, 8 -> build_fsub value data.one_f64 "fdecr" bldr
         | _ ->
            Report.err_internal __FILE__ __LINE__
                                "Tried to decrement unknown type."
       in
       let _ = build_store decr_result addr bldr in
       if pre_p then decr_result else value
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
                           ("llvm_unop not fully implemented: operator "
                            ^ (string_of_operator op))
  and llvm_binop pos op is_atomic tp left right bldr =
    let standard_op fcn name =
      let lhs = expr_gen true left in
      let rhs = expr_gen true right in
      if !Config.debug_symbols then
        let dbg_loc = make_debug_location pos in
        let () = set_current_debug_location data.bldr dbg_loc in 
        let llval = fcn lhs rhs name bldr in
        let () = clear_current_debug_location data.bldr in
        llval
      else
        fcn lhs rhs name bldr
    in
    let maybe_atomic_op nonatomic_fcn m_atomic_op name =

      let rhs = expr_gen true right in
      let left_addr = expr_gen false left in
      if is_atomic then
        let atomic_op = Util.the m_atomic_op in
        let atomic_val =
          build_atomicrmw atomic_op left_addr rhs
                          AtomicOrdering.AcqiureRelease
                          (* FIXME: misspelling of Acquire.  Contact LLVM. *)
                          false
                          name
                          bldr
        in
        nonatomic_fcn atomic_val rhs (name ^ ".val") bldr
      else
        let lhs = build_load_wrapper left_addr tp "deref" bldr in
        let res = nonatomic_fcn lhs rhs name bldr in
        let _ = build_store res left_addr bldr in
        res
    in
    let is_wildcard v = if v = Expr_Wildcard then true else false in

    let basetp = concrete_of data.prog.deftypemap tp in
    let integer_math = (is_integer_type basetp) || (is_pointer_type basetp) in
    let is_signed = if is_integer_type basetp then signed_p basetp else false
    in
    match op, integer_math with
    (* FIXME: Distinguish between signed/unsigned integers. *)
    | OperMult, true -> standard_op build_mul "def_mult"
    | OperMult, false -> standard_op build_fmul "def_mult_f"
    | OperDiv, true -> standard_op build_sdiv "def_sdiv"
    | OperDiv, false -> standard_op build_fdiv "def_div_f"
    | OperRemainder, true ->
       let op = if signed_p tp then build_srem else build_urem in
       standard_op op "def_rem"
    | OperLShift, true -> standard_op build_shl "def_shl"
    | OperRShift, true -> standard_op build_lshr "def_lshr"
    (* FIXME: use ashr for signed shift. *)
    | OperPlus, true -> standard_op build_add "def_add"
    | OperPlus, false -> standard_op build_fadd "def_add_f"
    | OperMinus, true -> standard_op build_sub "def_sub"
    | OperMinus, false -> standard_op build_fsub "def_sub_f"
    | OperLT, true ->
       let icmp = if is_signed then Icmp.Slt else Icmp.Ult in
       standard_op (build_icmp icmp) "def_lt"
    | OperLT, false -> standard_op (build_fcmp Fcmp.Olt) "def_lt_f"
    | OperLTE, true ->
       let icmp = if is_signed then Icmp.Sle else Icmp.Ule in
       standard_op (build_icmp icmp) "def_le"
    | OperLTE, false -> standard_op (build_fcmp Fcmp.Ole) "def_le_f"
    | OperGT, true ->
       let icmp = if is_signed then Icmp.Sgt else Icmp.Ugt in
       standard_op (build_icmp icmp) "def_gt"
    | OperGT, false -> standard_op (build_fcmp Fcmp.Ogt) "def_gt_f"
    | OperGTE, true ->
       let icmp = if is_signed then Icmp.Sge else Icmp.Uge in
       standard_op (build_icmp icmp) "def_ge"
    | OperGTE, false -> standard_op (build_fcmp Fcmp.Oge) "def_ge_f"
    | OperEquals, true ->
       if is_wildcard left || is_wildcard right then data.one_i8
       else standard_op (build_icmp Icmp.Eq) "def_eq"
    | OperEquals, false ->
       if is_wildcard left || is_wildcard right then data.one_i8
       else standard_op (build_fcmp Fcmp.Oeq) "def_eq_f"
    | OperNEquals, true -> standard_op (build_icmp Icmp.Ne) "def_neq"
    | OperNEquals, false -> standard_op (build_fcmp Fcmp.One) "def_neq_f"
    | OperBitwiseAnd, true -> standard_op build_and "def_band"
    | OperBitwiseOr, true -> standard_op build_or "def_bor"
    | OperBitwiseXor, true -> standard_op build_xor "def_bxor"
    | OperLogicalAnd, true -> standard_op build_and "def_land" (* FIXME!!! *)
    | OperAssign, _ ->
       begin match left, right with
       | Expr_StaticStruct (_, members), _ ->
          let rhs = expr_gen false right in
          let member_assign n (_, expr) =
            let lhs = expr_gen false expr in
            let field_name = "field_" ^ (string_of_int n) in
            let right_value = build_extractvalue rhs n field_name bldr in
            ignore (build_store right_value lhs bldr)
          in
          let () = List.iteri member_assign members in
          rhs
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
       | _, Expr_StaticArray exprs ->
          let base = expr_gen false left in
          let assign n expr =
            let v = expr_gen true expr in
            let loc =
              build_in_bounds_gep base [| data.zero_i32; (constval n) |]
                                  ("off" ^ (string_of_int n))
                                  bldr
            in
            ignore(build_store v loc bldr)
          in
          let () = List.iteri assign exprs in
          base (* FIXME: Same as above. *)
       | _, Expr_Cast (_, _, Expr_StaticArray _) ->
          Report.err_internal __FILE__ __LINE__
                              "Cast a whole static array.  That's bad."
       | _ ->
          let rhs = expr_gen true right in
          let _ = build_store rhs (expr_gen false left) bldr in
          rhs
       end
    | OperBitwiseAnd, false
    | OperBitwiseOr, false
    | OperBitwiseXor, false ->
       Report.err_internal __FILE__ __LINE__
                           ("tried to perform an operation \""
                            ^ (string_of_operator op)
                            ^ ") on float operands.  "
                            ^ "This should have been caught earlier.")
    | OperPlusAssign, _ ->
       maybe_atomic_op (if integer_math then build_add else build_fadd)
                       (Some AtomicRMWBinOp.Add)
                       "pluseq"
    | OperMinusAssign, _ ->
       maybe_atomic_op (if integer_math then build_sub else build_fsub)
                       (Some AtomicRMWBinOp.Sub)
                       "minuseq"
    | OperMultAssign, _ ->
       maybe_atomic_op (if integer_math then build_mul else build_fmul)
                       None (* atomic not implemented, yet. *)
                       "mult_eq"
    | OperBAndAssign, true ->
       maybe_atomic_op build_and (Some AtomicRMWBinOp.And) "andeq"
    | OperBOrAssign, true ->
       maybe_atomic_op build_or (Some AtomicRMWBinOp.Or) "oreq"
    | OperBXorAssign, true ->
       maybe_atomic_op build_xor (Some AtomicRMWBinOp.Xor) "oreq"
    | _ ->
       Report.err_internal __FILE__ __LINE__
         ("llvm_operator not fully implemented: operator "
          ^ (string_of_operator op))

  and make_llvm_tp deftp =
    match deftp.bare with
    | DefTypeFcn (params, ret, is_va) ->
       let llparams = Array.of_list @@ List.map make_llvm_tp params
       and llret = make_llvm_tp ret in
       if is_va then var_arg_function_type llret llparams
       else function_type llret llparams
    | DefTypePtr t ->
       pointer_type (make_llvm_tp t)
    | DefTypeNamed nm ->
       the (lookup_symbol data.typemap nm)
    | DefTypeEnum _ ->
       let sz = size_of data.prog.deftypemap deftp in
       integer_type data.ctx (sz * 8)
    | DefTypePrimitive pt ->
       the (lookup_symbol data.typemap (primitive2string pt))
    | DefTypeOpaque _
    | DefTypeVoid ->
       the (lookup_symbol data.typemap "i8")
    | _ -> Report.err_internal __FILE__ __LINE__
       ("make_llvm_tp incomplete: " ^ (string_of_type deftp))

  and build_cast raw_from raw_to e =
    let from_tp = concrete_of data.prog.deftypemap raw_from
    and to_tp = concrete_of data.prog.deftypemap raw_to
    in
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
    match from_tp.bare, to_tp.bare with
    | DefTypePrimitive prim1, DefTypePrimitive prim2 ->
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
    | DefTypeStaticStruct _, DefTypeNamed _
    | DefTypeLiteralStruct _, DefTypeNamed _ ->
       build_bitcast e (make_llvm_tp to_tp) "scast" data.bldr
    | DefTypeStaticStruct _, _
    | DefTypeLiteralStruct _, _ ->
       (* FIXME: I think this might be correct, actually, but need to think
          about it some more. *)
       e
    | DefTypeArray (tp, n), DefTypePtr _ ->
       build_in_bounds_gep e [|data.zero_i32; data.zero_i32|]
                           "ptr_cast" data.bldr
    | DefTypeFcn _, DefTypeFcn _ ->
       build_pointercast e (make_llvm_tp (makeptr to_tp)) "fcast" data.bldr
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
       let obj = build_cast (makeptr (maketype None DefTypeVoid))
                            (makeptr tp)
                            mem
       in
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
         | _ -> build_load_wrapper var tp "callee" data.bldr
       in
       let retname = match tp.bare with
         | DefTypeFcn (_, { bare = DefTypeVoid }, _) -> ""
         | _ -> "def_call"
       in
       build_call callee (Array.of_list arg_vals) retname data.bldr
    | Expr_FcnCall_Refs (nm, args) ->
       let arg_vals =
         List.map Util.the (List.map (lookup_symbol llvals) args) in
       let (_, tp, var) = the (lookup_symbol varmap nm) in
       (* Protect programmers from this function pointer nonsense. *)
       let callee = match classify_value var with
         | ValueKind.Function -> var
         | _ -> build_load_wrapper var tp "callee" data.bldr
       in
       let retname = match tp.bare with
         | DefTypeFcn (_, { bare = DefTypeVoid }, _) -> ""
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
       | Some (_, { bare = DefTypeFcn _ }, llvar) ->
          if not rvalue_p then llvar
          else begin match classify_value llvar with
               | ValueKind.Function -> llvar
               | _ -> build_load llvar "deref_fcn" data.bldr
                                 (* FIXME: volatile ptr? *)
               end
       | Some (_, { bare = DefTypeArray _ }, llvar) ->
          (* Arrays need to be treated differently from pointers - they
             should not be dereferenced here.  The array llvar _is_ the
             pointer to the memory, not a pointer to the pointer. *)
          llvar
       | Some (_, tp, llvar) ->
          if rvalue_p then build_load_wrapper llvar tp name data.bldr
          else llvar
       end
    | Expr_Binary (pos, op, is_atomic, tp, left, right) ->
       llvm_binop pos op is_atomic tp left right data.bldr
    | Expr_Unary (op, tp, expr, pre_p) ->
       llvm_unop op tp expr pre_p data.bldr
    | Expr_Cast (_, to_tp, Expr_Nil) ->
       const_null (make_llvm_tp to_tp)
    | Expr_Cast (from_tp, to_tp, expr) ->
       let e = expr_gen true expr in
       build_cast from_tp to_tp e
    | Expr_Index (base, idx, tp, deref_base, array, is_volatile) ->
       let i = expr_gen true idx in
       let b = expr_gen deref_base base in
       let () = if is_volatile then set_volatile true b in
       let addr =
         if not array then build_gep b [|i|] "addr" data.bldr
         else build_in_bounds_gep b [|data.zero_i32; i|] "addr" data.bldr
       in
       if rvalue_p then build_load_wrapper addr tp "deref" data.bldr
       else addr
    | Expr_SelectField (expr, n, is_volatile) ->
       let base = expr_gen false expr in
       let addr = build_struct_gep base n "maddr" data.bldr in
       if rvalue_p then
         let instr = build_load addr "mval" data.bldr in
         let () = if is_volatile then set_volatile true instr in
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
       let dest = expr_gen true dexpr in
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
       let dest = expr_gen true dexpr in
       let v = expr_gen true vexpr in
       build_atomicrmw AtomicRMWBinOp.Xchg dest v
                       AtomicOrdering.Acquire
                       false
                       "swap"
                       data.bldr
    | Expr_Atomic (AtomicSwap, _) ->
       Report.err_internal __FILE__ __LINE__ "Swap on != 2 parameters"
    | Expr_Val_Ref str ->
       Util.the (lookup_symbol llvals str)
    | Expr_StaticArray _ ->
       let pos, _ = pos_n_expr in
       let () = prerr_endline @@ Error.format_position pos in
       Report.err_internal __FILE__ __LINE__ "static array."
    | Expr_Wildcard ->
       Report.err_internal __FILE__ __LINE__ "wildcard."
  in
  let _, expr = pos_n_expr in
  expr_gen true expr

let get_debug_type =
  let debug_types = Hashtbl.create 32 in
  fun data ->
  let ctx = data.ctx in
  let dib = Util.the data.dib in
  let rec lookup_type tp =
    try Hashtbl.find debug_types tp
    with _ ->
         let create t =
           Hashtbl.add debug_types tp t;
           t
         in
         match tp.bare with
         | DefTypeFcn (params, ret, (*variadic:*)_) ->
            let plist = List.map lookup_type params in
            let r = lookup_type ret in
            create (disubroutine_type ctx dib (r :: plist))
         | DefTypePrimitive p ->
            let sz, dtype = dwarf_of tp in
            create (dibasic_type ctx dib (primitive2string p) sz dtype)
         | DefTypePtr p ->
            let base_type = lookup_type p in
            create (dipointer_type ctx dib base_type 64)
         | _ ->
            Report.err_internal __FILE__ __LINE__ "Incomplete debug type info."
  in
  lookup_type

let fcn_symbols data decl defn llfcn =
  let file = Util.the data.difile in
  let fcn_md = difunction data.ctx
                          (Util.the data.dib)
                          defn.name
                          file
                          (*scope=*)file
                          defn.defn_begin.pos_lnum
                          (decl.vis = VisLocal)
                          (get_debug_type data decl.tp)
  in
  set_subprogram llfcn fcn_md;
  Hashtbl.add debug_locations (ScopeGlobal defn.defn_begin) fcn_md

let process_fcn data symbols fcn =
  let profile = the (lookup_symbol data.prog.global_decls fcn.name) in
  let (_, _, llfcn) = the (lookup_symbol symbols profile.mappedname) in
  let () = if !Config.debug_symbols then fcn_symbols data profile fcn llfcn in
  let llblocks = Hashtbl.create 32 in
  let make_llbbs () = function
    | BB_Seq (label, _)
    | BB_Cond (label, _)
    | BB_Term (label, _)
    | BB_Detach (label, _, _)
    | BB_Reattach (label, _,  _)
    | BB_Sync (label, _, _)
      ->
       Hashtbl.add llblocks label (append_block data.ctx label llfcn)
    | _ -> Report.err_internal __FILE__ __LINE__ "Unexpected block type."
  in

  let varmap = push_symtab_scope symbols in
  let () = Cfg.reset_bbs fcn.entry_bb in
  let () = Cfg.visit_df make_llbbs true () fcn.entry_bb in

  let llvals = make_symtab () in

  let make_sync_region sync_region =
    let _, _, syncreg_start =
      (* FIXME: Gather builtin names together in a table. *)
      the (lookup_symbol varmap "llvm.syncregion.start") in
    let ret = build_call syncreg_start [| |] "sync_region" data.bldr in
    add_symbol llvals sync_region ret
  in

  let get_bb = Hashtbl.find llblocks in

  let get_label = function
    | BB_Seq (label, _)
    | BB_Cond (label, _)
    | BB_Term (label, _)
    | BB_Detach (label, _, _)
    | BB_Reattach (label, _, _)
    | BB_Sync (label, _, _)
      -> label
    | BB_Error ->
       Report.err_internal __FILE__ __LINE__ "Found an error block."
    | _ -> Report.err_internal __FILE__ __LINE__ "Unexpected block type."
  in

  let add_parallel_loop_metadata bb =
    let kind = mdkind_id data.ctx "llvm.loop" in
    let tapir_str = mdstring data.ctx "tapir.loop.spawn.strategy" in
    let strategy = data.one_i32 in
    let tapirmd = mdnode data.ctx [| tapir_str; strategy |] in
    set_metadata bb kind tapirmd
  in

  let populate_blocks () = function
    | BB_Seq (label, block) ->
       let bb = get_bb label in
       let () = position_at_end bb data.bldr in
       let () = List.iter
         (fun expr ->
           ignore (process_expr data llvals varmap expr)) block.seq_expr in
       let next_bb = get_bb (get_label block.seq_next) in
       let _ = build_br next_bb data.bldr in
       ()
    | BB_Cond (label, block) ->
       let bb = get_bb label in
       let () = position_at_end bb data.bldr in
       let cond = process_expr data llvals varmap block.cond_branch
       and then_bb = get_bb (get_label block.cond_next)
       and else_bb = get_bb (get_label block.cond_else) in
       let cond_bb = build_cond_br cond then_bb else_bb data.bldr in
       if block.cond_parallel then
         add_parallel_loop_metadata cond_bb
    | BB_Term (label, block) ->
       let make_xend () =
         let fname = match !Config.xact_kind with
           | Config.XACT_HARDWARE -> "llvm.x86.xend"
           | Config.XACT_HYBRID -> "__defrts_hybrid_xend"
           | Config.XACT_SOFTWARE ->
              Report.err_internal __FILE__ __LINE__ "STM not supported, yet."
         in
         let _, _, xend = the @@ lookup_symbol varmap fname in
         ignore(build_call xend [| |] "" data.bldr)
       in
       let bb = get_bb label in
       let () = position_at_end bb data.bldr in
       begin match block.term_expr with
       | None ->
          let () = if block.term_xend then make_xend () in
          let _ = build_ret_void data.bldr in ()
       | Some e ->
          let retexpr = process_expr data llvals varmap e in
          let () = if block.term_xend then make_xend () in
          let _ = build_ret retexpr data.bldr in ()
       end
    | BB_Detach (label, sb, block) ->
       let bb = get_bb label in
       let () = position_at_end bb data.bldr in
       let sync_region = the (lookup_symbol llvals sb) in
       let proc (id, expr) =
         let llval = process_expr data llvals varmap (faux_pos, expr) in
         add_symbol llvals id llval
       in
       let () = List.iter proc block.detach_args in
       let () = if block.detach_ret <> None then
                  proc (Util.the block.detach_ret) in
       let spawn_bb = get_bb (get_label block.detach_next) in
       let cont_bb = get_bb (get_label block.detach_continuation) in
       ignore(build_detach spawn_bb cont_bb sync_region data.bldr)
    | BB_Reattach (label, sb, block) ->
       let bb = get_bb label in
       let () = position_at_end bb data.bldr in
       let sync_region = the (lookup_symbol llvals sb) in
       let () = List.iter
                  (fun expr ->
                    ignore(process_expr data llvals varmap expr))
                  block.seq_expr in
       let next_bb = get_bb (get_label block.seq_next) in
       ignore(build_reattach next_bb sync_region data.bldr)
    | BB_Sync (label, sb, block) ->
       let bb = get_bb label in
       let () = position_at_end bb data.bldr in
       let sync_region = the (lookup_symbol llvals sb) in
       let next_bb = get_bb (get_label block.seq_next) in
       ignore(build_sync next_bb sync_region data.bldr)
    | _ -> Report.err_internal __FILE__ __LINE__ "Unexpected block type."
  in

  let entry = get_bb "entry" in
  position_at_end entry data.bldr;

  (* Arguments. *)
  let (args, _, _) = get_fcntype profile.tp.bare in
  let llparams = params llfcn in
  List.iteri (fun i ((pos, n), tp) ->
    let alloc =
      build_alloca (deftype2llvmtype data true tp) n data.bldr
    in begin
      add_symbol varmap n (pos, tp, alloc);
      ignore (build_store llparams.(i) alloc data.bldr);
    end) (List.combine profile.params args);

  (* Space for local variables. *)
  List.iter (fun (name, decl) ->
    let alloc = build_alloca
      (deftype2llvmtype data true decl.tp) name data.bldr
    in add_symbol varmap name (decl.decl_pos, decl.tp, alloc))
    fcn.local_vars;

  (* Initialize Cilk sync regions. *)
  List.iter make_sync_region fcn.fcn_cilk_init;

  Cfg.visit_df populate_blocks false () fcn.entry_bb;
  List.iter
    (fun attr -> add_function_attr llfcn attr AttrIndex.Function)
    data.fattrs;
  if not !Config.codegen_debug then Llvm_analysis.assert_valid_function llfcn

let rec get_const_val data = function
  | Expr_String (_, str) -> const_stringz data.ctx str
  | Expr_Literal astlit -> process_literal data.typemap astlit
  | Expr_Unary (OperMinus, tp, expr, true) ->
     const_neg (get_const_val data expr)
  | Expr_Cast (from, totp, e) ->
     (prerr_endline ("cast " ^ (string_of_type from) ^ " to "
                     ^ (string_of_type totp));
      Report.err_internal __FILE__ __LINE__ "no cast initializers.  Sorry.")
  | _ -> Report.err_internal __FILE__ __LINE__
                             "Not implemented case of get_const_val"

let zero_llval data = function
  | DefTypePrimitive prim ->
     let primzero = match prim with
       | PrimBool -> LitBool false
       | PrimI8 -> LitI8 (Char.chr 0)
       | PrimU8 -> LitU8 (Char.chr 0)
       | PrimI16 -> LitI16 (Int32.of_int 0)
       | PrimU16 -> LitU16 (Int32.of_int 0)
       | PrimI32 -> LitI32 (Int32.of_int 0)
       | PrimU32 -> LitU32 (Int32.of_int 0)
       | PrimI64 -> LitI64 (Int64.of_int 0)
       | PrimU64 -> LitU64 (Int64.of_int 0)
       | PrimF32 -> LitF32 0.0
       | PrimF64 -> LitF64 0.0
     in
     process_literal data.typemap primzero
  | _ -> Report.err_internal __FILE__ __LINE__ "Unknown zero val"

let declare_globals data symbols initializers name decl =
  let lltp = deftype2llvmtype data false decl.tp in
  let should_declare =
    match decl.decl_ref, decl.vis with
    | true, _ -> true
    | false, VisExported _ -> true
    | _ -> false
  in
  if should_declare then
    let llval = match decl.tp.bare with
      | DefTypeFcn _ ->
         let llfcn = declare_function decl.mappedname lltp data.mdl in
         (if decl.vis = VisLocal then set_linkage Linkage.Internal llfcn;
          llfcn)
      | _ ->
         let llglobal =
           try
             let init = match Hashtbl.find initializers decl.mappedname with
               | Expr_Binary (_, OperAssign, _, _, _, rhs) ->
                  get_const_val data rhs
               | _ -> Report.err_internal __FILE__ __LINE__
                                          "Init that is not an assignment."
             in
             define_global decl.mappedname init data.mdl
           with _ ->
             if decl.vis = VisExternal then
               let llglobal = declare_global lltp name data.mdl in
               let () = set_externally_initialized true llglobal in
               llglobal
             else
               define_global decl.mappedname
                             (zero_llval data decl.tp.bare)
                             data.mdl
         in
         let () = if decl.is_tls then set_thread_local true llglobal in
         llglobal
    in
    add_symbol symbols decl.mappedname (decl.decl_pos, decl.tp, llval)

let make_module_flags data =
  if !Config.position_indep then
    let pic_level =
      mdnode data.ctx
             [| const_int (i32_type data.ctx) 1;
                mdstring data.ctx "PIC Level";
                const_int (i32_type data.ctx) 2 |]
    in
    add_named_metadata_operand data.mdl "llvm.module.flags" pic_level

let debug_sym_preamble data module_name =
  let i32 = const_int (i32_type data.ctx) in
  let version_string =
    (string_of_int Version.version_maj)
    ^ "." ^ (string_of_int Version.version_min)
    ^ "." ^ (string_of_int Version.version_patch)
    ^ Version.version_suffix
  in
  let defident = "def version " ^ version_string in
  let dib = dibuilder data.mdl in
  (* FIXME: basename should be expanded so it isn't "." or whatever. *)
  let file = difile data.ctx dib (Filename.basename module_name)
                    (Filename.dirname module_name)
  in
  let _ = dicompile_unit data.ctx dib file defident
                         (!Config.opt_level <> 0) "" 0 in
  data.difile <- Some file;
  data.dib <- Some dib;
  let dwarf_version =
    mdnode data.ctx [| i32 2; mdstring data.ctx "Dwarf Version"; i32 4 |]
  in
  let debug_info_version =
    mdnode data.ctx [| i32 2; mdstring data.ctx "Debug Info Version"; i32 3 |]
  in
  add_named_metadata_operand data.mdl "llvm.module.flags" dwarf_version;
  add_named_metadata_operand data.mdl "llvm.module.flags" debug_info_version;
  add_named_metadata_operand data.mdl "llvm.ident" (mdstring data.ctx defident)

let process_cfg module_name program =
  let ctx  = global_context () in
  let mdl  = create_module ctx module_name in
  let ()   = set_target_triple (Target.default_triple ()) mdl in
  let bldr = builder ctx in
  let typemap = build_types ctx program.deftypemap in
  let data = { ctx = ctx;
               mdl = mdl;
               bldr = bldr;
               typemap = typemap;
               prog = program;
               fattrs = List.map
                          (fun (key, value) ->
                            create_string_attr ctx key value)
                          fcn_attrs;
               difile = None;
               dib = None;
               d_scope_table = program.scope_table;
               zero_i32 = const_null (the (lookup_symbol typemap "i32"));
               one_i8 = const_int (the (lookup_symbol typemap "bool")) 1;
               one_i16 = const_int (the (lookup_symbol typemap "i16")) 1;
               one_i32 = const_int (the (lookup_symbol typemap "i32")) 1;
               one_i64 = const_int (the (lookup_symbol typemap "i64")) 1;
               one_f32 = const_float (the (lookup_symbol typemap "f32")) 1.0;
               one_f64 = const_float (the (lookup_symbol typemap "f64")) 1.0
             } in
  make_module_flags data;
  if !Config.debug_symbols then debug_sym_preamble data module_name;
  let symbols = make_symtab () in
  symtab_iter (declare_globals data symbols program.initializers)
              program.global_decls;
  List.iter (process_fcn data symbols) program.fcnlist;
  mdl
