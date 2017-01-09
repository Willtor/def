open Ast
open Cfg
open Llvm
open Types
open Util

type llvm_data =
  { ctx  : llcontext;
    mdl  : llmodule;
    bldr : llbuilder;
    typemap : lltype symtab;
    prog : program;
    zero_i32 : llvalue
  }

let get_fcntype = function
  | DefTypeFcn (params, ret) -> params, ret
  | _ -> Report.err_internal __FILE__ __LINE__ "Expected a DefTypeFcn."

let deftype2llvmtype ctx typemap =
  let rec convert wrap_fcn_ptr = function
    | DefTypeUnresolved (_, name) ->
       Report.err_internal __FILE__ __LINE__
         ("Tried to convert a placeholder type: " ^ name)
    | DefTypeVoid ->
       the (lookup_symbol typemap "void")
    | DefTypeFcn (args, ret) ->
       let llvmargs = List.map (fun argtp -> convert true argtp) args in
       let ftype = function_type (convert true ret) (Array.of_list llvmargs)
       in
       if wrap_fcn_ptr then pointer_type ftype
       else ftype
    | DefTypePrimitive prim ->
       let name = primitive2string prim in
       the (lookup_symbol typemap name)
    | DefTypePtr pointed_to_tp ->
       pointer_type (convert wrap_fcn_ptr pointed_to_tp)
    | DefTypeNamedStruct name ->
       the (lookup_symbol typemap name)
    | DefTypeLiteralStruct (members, _) ->
       let llvm_members = List.map (convert wrap_fcn_ptr) members in
       struct_type ctx (Array.of_list llvm_members)
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
    (fun (name, _, f) -> add_symbol typemap name (f ctx))
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

let process_expr data varmap =
  let rec llvm_unop op (*tp*)_ expr (*pre_p*)_ bldr =
    match op with
    | OperAddrOf ->
       let llvm_expr = expr_gen false expr in
       build_gep llvm_expr [| data.zero_i32 |] "addrof" bldr
    | _ -> failwith "llvm_unop not fully implemented."
  and llvm_binop op tp left right bldr =
    let standard_op fnc name =
      fnc (expr_gen true left) (expr_gen true right) name bldr
    in
    match op, is_integer_type tp with
    (* FIXME: Distinguish between signed/unsigned integers. *)
    | OperMult, true -> standard_op build_mul "def_mult"
    | OperMult, false -> standard_op build_fmul "def_mult_f"
    | OperDiv, true -> standard_op build_sdiv "def_sdiv"
    | OperDiv, false -> standard_op build_fdiv "def_div_f"
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
    | OperAssign, _ ->
       let rhs = expr_gen true right in
       let _ = build_store rhs (expr_gen false left) bldr in
       rhs
    | _ -> failwith "llvm_operator not fully implemented"

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
    | DefTypePrimitive prim1, DefTypePrimitive prim2 ->
       build_primitive_cast prim1 prim2
    | _ -> failwith "Irfactory.build_cast incomplete implementation."

  and expr_gen rvalue_p = function
    | Expr_FcnCall (name, args) ->
       let (_, tp, var) = the (lookup_symbol varmap name) in
       let arg_vals = List.map (expr_gen true) args in
       (* Protect programmers from this function pointer nonsense. *)
       let callee = match classify_value var with
         | ValueKind.Function -> var
         | _ -> build_load var "callee" data.bldr
       in
       let retname = match tp with
         | DefTypeFcn (_, DefTypeVoid) -> ""
         | _ -> "def_call"
       in
       build_call callee (Array.of_list arg_vals) retname data.bldr
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
       | Some (_, _, llvar) ->
          if rvalue_p then build_load llvar name data.bldr
          else llvar
       end
    | Expr_Binary (op, tp, left, right) ->
       llvm_binop op tp left right data.bldr
    | Expr_Unary (op, tp, expr, pre_p) ->
       llvm_unop op tp expr pre_p data.bldr
    | Expr_Cast (from_tp, to_tp, expr) ->
       let e = expr_gen true expr in
       build_cast from_tp to_tp e
    | Expr_Index (base, idx) ->
       let i = expr_gen true idx in
       let b = expr_gen true base in
       let addr = build_gep b [|i|] "idx" data.bldr in
       if rvalue_p then build_load addr "idxval" data.bldr
       else addr
    | Expr_SelectField (expr, n) ->
       let base = expr_gen false expr in
       let addr = build_struct_gep base n "maddr" data.bldr in
       if rvalue_p then build_load addr "mval" data.bldr
       else addr
  in expr_gen true

let rec process_body data llfcn varmap cfg_bbs entry_bb =
  let process_bb bb = function
    | BB_Cond (_, conditional) ->
       let cond = process_expr data varmap conditional.branch_cond in

       (* then-branch *)
       let then_start = append_block data.ctx "then" llfcn in
       let () = position_at_end then_start data.bldr in
       let then_end =
         process_body data llfcn varmap conditional.then_scope then_start in

       (* else-branch *)
       let else_start = append_block data.ctx "else" llfcn in
       let () = position_at_end else_start data.bldr in
       let else_end =
         process_body data llfcn varmap conditional.else_scope else_start in

       (* conditional *)
       let () = position_at_end bb data.bldr in
       let _ = build_cond_br cond then_start else_start data.bldr in

       if conditional.then_returns && conditional.else_returns then else_end
       else let merge_bb = append_block data.ctx "merge" llfcn in
            begin
              position_at_end merge_bb data.bldr;
              if not conditional.then_returns then
                begin
                  position_at_end then_end data.bldr;
                  ignore (build_br merge_bb data.bldr)
                end;
              if not conditional.else_returns then
                begin
                  position_at_end else_end data.bldr;
                  ignore (build_br merge_bb data.bldr)
                end;
              position_at_end merge_bb data.bldr;
              merge_bb
            end
    | BB_Loop (_, block) ->

       (* Check whether the loop is a while or a do-while loop. *)
       if block.precheck then
         (* New block for the condition. *)
         let cond_bb = append_block data.ctx "loop_cond" llfcn in
         let _ = build_br cond_bb data.bldr in
         let () = position_at_end cond_bb data.bldr in
         let cond = process_expr data varmap block.loop_cond in

         (* Loop body. *)
         let body_bb = append_block data.ctx "loop_body" llfcn in
         let () = position_at_end body_bb data.bldr in
         let _ = process_body data llfcn varmap block.body_scope body_bb in
         let _ = build_br cond_bb data.bldr in

         (* Follow block. *)
         let follow_bb = append_block data.ctx "loop_follow" llfcn in
         let () = position_at_end cond_bb data.bldr in
         let _ = build_cond_br cond body_bb follow_bb data.bldr in
         let () = position_at_end follow_bb data.bldr in
         follow_bb
       else
         (* Loop body. *)
         let body_bb = append_block data.ctx "loop_body" llfcn in
         let _ = build_br body_bb data.bldr in
         let () = position_at_end body_bb data.bldr in
         let _ = process_body data llfcn varmap block.body_scope body_bb in

         (* Block for the conditional. *)
         let cond_bb = append_block data.ctx "loop_cond" llfcn in
         let _ = build_br cond_bb data.bldr in
         let () = position_at_end cond_bb data.bldr in
         let cond = process_expr data varmap block.loop_cond in

         (* Follow block. *)
         let follow_bb = append_block data.ctx "loop_follow" llfcn in
         let _ = build_cond_br cond body_bb follow_bb data.bldr in
         let () = position_at_end follow_bb data.bldr in
         follow_bb

    | BB_Expr (_, _, expr) ->
       begin ignore (process_expr data varmap expr); bb end
    | BB_Return (_, expr) ->
       let ret = process_expr data varmap expr in
       let _ = build_ret ret data.bldr in
       bb
    | BB_ReturnVoid _ ->
       let _ = build_ret_void data.bldr in bb
    | BB_LocalFcn fcn ->
       Report.err_internal __FILE__ __LINE__
         ("Unlifted function: " ^ fcn.name)
  in
  List.fold_left process_bb entry_bb cfg_bbs

let process_fcn data symbols fcn =
  let profile = the (lookup_symbol data.prog.global_decls fcn.name) in
  let (_, _, llfcn) = the (lookup_symbol symbols profile.mappedname) in
  let varmap = push_symtab_scope symbols in
  let bb = append_block data.ctx "entry" llfcn in
  position_at_end bb data.bldr;
  let (args, _) = get_fcntype profile.tp in
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
  ignore (process_body data llfcn varmap fcn.bbs bb);
  Llvm_analysis.assert_valid_function llfcn

let declare_globals data symbols name decl =
  let llfcn =
    (* FIXME: Might not be a function.  Need to check decl.tp. *)
    declare_function
      decl.mappedname (deftype2llvmtype data.ctx data.typemap false decl.tp)
      data.mdl
  in
  if decl.vis = VisLocal then set_linkage Linkage.Internal llfcn;
  add_symbol symbols decl.mappedname (decl.decl_pos, decl.tp, llfcn)

let process_cfg module_name program =
  let ctx  = global_context () in
  let mdl  = create_module ctx module_name in
  let bldr = builder ctx in
  let typemap = build_types ctx program.deftypemap in
  let data = { ctx = ctx;
               mdl = mdl;
               bldr = bldr;
               typemap = typemap;
               prog = program;
               zero_i32 = const_null (the (lookup_symbol typemap "i32")) } in
  let symbols = make_symtab () in
  symtab_iter (declare_globals data symbols) program.global_decls;
  List.iter (process_fcn data symbols) program.fcnlist;
  mdl
