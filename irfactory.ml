open Ast
open Cfg
open Llvm
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
  | FcnType (params, ret) -> params, ret
  | _ -> fatal_error "Internal error.  Function not of FcnType."

let builtin_types ctx =
  let typemap = make_symtab () in
  List.iter
    (fun (name, _, _, f) -> add_symbol typemap name (f ctx))
    Types.map_builtin_types;
  typemap

let deftype2llvmtype typemap =
  let rec convert = function
    | FcnType (args, ret) ->
       let llvmargs = List.map (fun (_, _, argtp) -> convert argtp) args in
       function_type (convert ret) (Array.of_list llvmargs)
    | VarType (pos, typename) ->
       begin match lookup_symbol typemap typename with
       | Some t -> t
       | None ->
          let errstr = "Unknown type \"" ^ typename ^ "\": "
            ^ (format_position pos) ^ "\n"
            ^ (show_source pos)
          in raise (ProcessingError errstr)
       end
  in convert

let process_literal typemap = function
  | I32 n -> const_int (the (lookup_symbol typemap "i32")) (Int32.to_int n)
  | _ -> failwith "Irfactory.process_literal not fully implemented."

let process_variable varmap name =
  let (_, _, llvar) = the (lookup_symbol varmap name) in llvar

let process_expr data varmap =
  let llvm_operator = function
    (* FIXME: Should specify a proper name for intermediate values. *)
    | OperMult _ -> (build_mul, "def_mult")
    | OperDiv _ -> (build_sdiv, "def_sdiv")
    | OperPlus _ -> (build_add, "def_add")
    | OperMinus _ -> (build_sub, "def_sub")
    | OperLT _ -> (build_icmp Icmp.Slt, "def_lt")
    | OperLTE _ -> (build_icmp Icmp.Sle, "def_le")
    | OperGT _ -> (build_icmp Icmp.Sgt, "def_gt")
    | OperGTE _ -> (build_icmp Icmp.Sge, "def_ge")
    | OperEquals _ -> (build_icmp Icmp.Eq, "def_eq")
    | _ -> failwith "llvm_operator not fully implemented"
  in
  let rec expr_gen = function
    | Expr_Literal lit -> process_literal data.typemap lit
    | Expr_Variable name ->
       let v = process_variable varmap name in
       build_load v name data.bldr
    | Expr_Binary (op, left, right) ->
       let e1 = expr_gen left
       and e2 = expr_gen right
       and (func, ident) = llvm_operator op in
       func e1 e2 ident data.bldr
    | _ -> failwith "expr_gen not fully implemented."
  in expr_gen

let rec process_body data llfcn varmap scope entry_bb =
  let process_bb bb = function
    | BB_Cond conditional ->
       let cond = process_expr data varmap conditional.cond in

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

    | BB_Expr (_, expr) ->
       begin ignore (process_expr data varmap expr); bb end
    | BB_Scope scope -> process_body data llfcn varmap scope bb
    | BB_Return (_, expr) ->
       let ret = process_expr data varmap expr in
       let _ = build_ret ret data.bldr in
       bb
    | BB_ReturnVoid _ ->
       failwith "FIXME: Not implemented, yet."
  in
  List.fold_left process_bb entry_bb scope.bbs

let process_fcn data fcn =
  let profile = the (lookup_symbol data.prog.global_decls fcn.name) in
  let varmap = make_symtab () in
  let llfcn =
    declare_function fcn.name
      (deftype2llvmtype data.typemap profile.tp) data.mdl
  in
  let bb = append_block data.ctx "entry" llfcn in
  position_at_end bb data.bldr;
  let (args, _) = get_fcntype profile.tp in
  let llparams = params llfcn in
  List.iteri (fun i (pos, n, tp) ->
    let alloc = build_alloca (deftype2llvmtype data.typemap tp) n data.bldr
    in begin
      add_symbol varmap n (pos, tp, alloc);
      ignore (build_store llparams.(i) alloc data.bldr);
    end) args;
  ignore (process_body data llfcn varmap fcn.body bb);
  Llvm_analysis.assert_valid_function llfcn

let process_cfg outfile module_name program =
  let ctx  = global_context () in
  let mdl  = create_module ctx module_name in
  let bldr = builder ctx in
  let typemap = builtin_types ctx in
  let data = { ctx = ctx; mdl = mdl; bldr = bldr;
               typemap = typemap; prog = program } in
  List.iter (process_fcn data) program.fcnlist;
  print_module outfile mdl
