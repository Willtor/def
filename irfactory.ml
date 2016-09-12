open Ast
open Cfg
open Llvm
open Types
open Util

exception ProcessingError of string

type llvm_data =
  { ctx  : llcontext;
    mdl  : llmodule;
    bldr : llbuilder;
    typemap : lltype symtab;
    prog : program
  }

let get_fcntype = function
  | DefTypeFcn (params, ret) -> params, ret
  | _ -> Report.err_internal __FILE__ __LINE__ "Expected a DefTypeFcn."

let builtin_types ctx =
  let typemap = make_symtab () in
  List.iter
    (fun (name, _, f) -> add_symbol typemap name (f ctx))
    Types.map_builtin_types;
  typemap

let deftype2llvmtype typemap =
  let rec convert = function
    | DefTypeFcn (args, ret) ->
       let llvmargs = List.map (fun argtp -> convert argtp) args in
       function_type (convert ret) (Array.of_list llvmargs)
    | DefTypePrimitive prim ->
       let name = primitive2string prim in
       the (lookup_symbol typemap name)
    | DefTypePtr pointed_to_tp ->
       pointer_type (convert pointed_to_tp)
    | DefTypeVoid ->
       the (lookup_symbol typemap "void")
  in convert

let process_literal typemap lit = match lit with
  | LitBool (_, true) ->
     const_int (the (lookup_symbol typemap "bool")) 1
  | LitBool (_, false) ->
     const_int (the (lookup_symbol typemap "bool")) 0
  | LitI16 (_, n)
  | LitU16 (_, n)
  | LitI32 (_, n)
  | LitU32 (_, n) ->
     let typename = primitive2string (literal2primitive lit) in
     const_int (the (lookup_symbol typemap typename)) (Int32.to_int n)
  | LitI64 (_, n) ->
     const_of_int64 (the (lookup_symbol typemap "i64")) n true
  | LitU64 (_, n) ->
     const_of_int64 (the (lookup_symbol typemap "i64")) n false

let process_variable varmap name =
  match lookup_symbol varmap name with
  | None -> Report.err_internal __FILE__ __LINE__
     ("Failed to find variable " ^ name ^ ".")
  | Some (_, _, llvar) -> llvar

let process_expr data varmap =
  let rec llvm_binop op left right bldr =
    let standard_op fnc name =
      fnc (expr_gen true left) (expr_gen true right) name bldr
    in
    match op with
    | OperMult _ -> standard_op build_mul "def_mult"
    | OperDiv _ -> standard_op build_sdiv "def_sdiv"
    | OperPlus _ -> standard_op build_add "def_add"
    | OperMinus _ -> standard_op build_sub "def_sub"
    | OperLT _ -> standard_op (build_icmp Icmp.Slt) "def_lt"
    | OperLTE _ -> standard_op (build_icmp Icmp.Sle) "def_le"
    | OperGT _ -> standard_op (build_icmp Icmp.Sgt) "def_gt"
    | OperGTE _ -> standard_op (build_icmp Icmp.Sge) "def_ge"
    | OperEquals _ -> standard_op (build_icmp Icmp.Eq) "def_eq"
    | OperAssign _ ->
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
        | PrimBool, PrimI16 | PrimBool, PrimU16
        | PrimBool, PrimI32 | PrimBool, PrimU32
        | PrimBool, PrimI64 | PrimBool, PrimU64 -> build_zext

        (* From i16 *)
        | PrimI16, PrimBool -> build_trunc
        | PrimI16, PrimU16 -> null_cast
        | PrimI16, PrimI32 | PrimI16, PrimI64 -> build_sext
        | PrimI16, PrimU32 | PrimI16, PrimU64 -> build_zext

        (* From u16 *)
        | PrimU16, PrimBool -> build_trunc
        | PrimU16, PrimI16 -> null_cast
        | PrimU16, PrimI32 | PrimU16, PrimU32
        | PrimU16, PrimI64 | PrimU16, PrimU64 -> build_zext

        (* From i32 *)
        | PrimI32, PrimBool
        | PrimI32, PrimI16 | PrimI32, PrimU16 -> build_trunc
        | PrimI32, PrimU32 -> null_cast
        | PrimI32, PrimI64 -> build_sext
        | PrimI32, PrimU64 -> build_zext

        (* From u32 *)
        | PrimU32, PrimBool
        | PrimU32, PrimI16 | PrimU32, PrimU16 -> build_trunc
        | PrimU32, PrimI32 -> null_cast
        | PrimU32, PrimI64 | PrimU32, PrimU64 -> build_zext

        (* From i64 *)
        | PrimI64, PrimBool
        | PrimI64, PrimI16 | PrimI64, PrimU16
        | PrimI64, PrimI32 | PrimI64, PrimU32 -> build_trunc
        | PrimI64, PrimU64 -> null_cast

        (* From u64 *)
        | PrimU64, PrimBool
        | PrimU64, PrimI16 | PrimU64, PrimU16
        | PrimU64, PrimI32 | PrimU64, PrimU32 -> build_trunc
        | PrimU64, PrimI64 -> null_cast

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
       let (_, _, callee) = the (lookup_symbol varmap name) in
       let arg_vals = List.map (expr_gen true) args in
       build_call callee (Array.of_list arg_vals) "def_call" data.bldr
    | Expr_Literal lit -> process_literal data.typemap lit
    | Expr_Variable name ->
       let v = process_variable varmap name in
       if rvalue_p then build_load v name data.bldr
       else v
    | Expr_Binary (op, left, right) ->
       llvm_binop op left right data.bldr
    | Expr_Cast (from_tp, to_tp, expr) ->
       let e = expr_gen true expr in
       build_cast from_tp to_tp e
    | Expr_Index (base, idx) ->
       let i = expr_gen true idx in
       let b = expr_gen true base in
       let addr = build_gep b [|i|] "idx" data.bldr in
       if rvalue_p then build_load addr "idxval" data.bldr
       else addr
    | _ -> failwith "expr_gen not fully implemented."
  in expr_gen true

let rec process_body data llfcn varmap cfg_bbs entry_bb =
  let process_bb bb = function
    | BB_Cond conditional ->
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
    | BB_Loop block ->
       (* New block for the condition. *)
       let cond_bb = append_block data.ctx "loop" llfcn in
       let _ = build_br cond_bb data.bldr in
       let () = position_at_end cond_bb data.bldr in
       let cond = process_expr data varmap block.loop_cond in

       (* Loop body. *)
       let body_bb = append_block data.ctx "loop_body" llfcn in
       let () = position_at_end body_bb data.bldr in
       let _ =
         process_body data llfcn varmap block.body_scope body_bb in
       let _ = build_br cond_bb data.bldr in

       (* Follow block. *)
       let follow_bb = append_block data.ctx "loop_follow" llfcn in
       let () = position_at_end cond_bb data.bldr in
       let _ = build_cond_br cond body_bb follow_bb data.bldr in
       let () = position_at_end follow_bb data.bldr in
       follow_bb

    | BB_Expr (_, expr) ->
       begin ignore (process_expr data varmap expr); bb end
    | BB_Return (_, expr) ->
       let ret = process_expr data varmap expr in
       let _ = build_ret ret data.bldr in
       bb
    | BB_ReturnVoid _ ->
       let _ = build_ret_void data.bldr in bb
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
    let alloc = build_alloca (deftype2llvmtype data.typemap tp) n data.bldr
    in begin
      add_symbol varmap n (pos, tp, alloc);
      ignore (build_store llparams.(i) alloc data.bldr);
    end) (List.combine profile.params args);
  List.iter (fun (name, decl) ->
    let alloc = build_alloca
      (deftype2llvmtype data.typemap decl.tp) name data.bldr
    in add_symbol varmap name (decl.decl_pos, decl.tp, alloc))
    fcn.local_vars;
  ignore (process_body data llfcn varmap fcn.bbs bb);
  Llvm_analysis.assert_valid_function llfcn

let declare_globals data symbols name decl =
  let llfcn =
    (* FIXME: Might not be a function.  Need to check decl.tp. *)
    declare_function decl.mappedname (deftype2llvmtype data.typemap decl.tp)
      data.mdl
  in
  add_symbol symbols decl.mappedname (decl.decl_pos, decl.tp, llfcn)

let process_cfg outfile module_name program =
  let ctx  = global_context () in
  let mdl  = create_module ctx module_name in
  let bldr = builder ctx in
  let typemap = builtin_types ctx in
  let data = { ctx = ctx; mdl = mdl; bldr = bldr;
               typemap = typemap; prog = program } in
  let symbols = make_symtab () in
  symtab_iter (declare_globals data symbols) program.global_decls;
  List.iter (process_fcn data symbols) program.fcnlist;
  print_module outfile mdl
