open Ast
open Llvm
open Util

exception ProcessingError of string

type llvm_data =
  { ctx  : llcontext;
    mdl  : llmodule;
    bldr : llbuilder
  }

type scope_data =
  { fcntable : (Lexing.position * vartype * llvalue) symtab;
    vartable : (Lexing.position * vartype * llvalue) symtab;
    typemap  : lltype symtab
  }

let push_scope_data scope =
  { fcntable = push_symtab_scope scope.fcntable;
    vartable = push_symtab_scope scope.vartable;
    typemap  = push_symtab_scope scope.typemap
  }

(** Set up builtin type definitions known to DEF and return them as a global
    struct. *)
let builtin_types ctx =
  let typemap = make_symtab () in
  begin
    add_symbol typemap "i32" (i32_type ctx);
    typemap
  end

(** Convert a named DEF type to its LLVM equivalent. *)
let deftype2llvmtype scope =
  let rec convert = function
    | FcnType (args, ret) ->
       let llvmargs = List.map (fun (_, _, argtp) -> convert argtp) args in
       function_type (convert ret) (Array.of_list llvmargs)
    | VarType (pos, typename) ->
       begin match lookup_symbol scope.typemap typename with
       | Some t -> t
       | None ->
          let errstr = "Unknown type \"" ^ typename ^ "\": "
            ^ (format_position pos) ^ "\n"
            ^ (show_source pos)
          in raise (ProcessingError errstr)
       end
  in convert

(** Gather the global names and types.  For simpler mutual recursion, DEF
    does not require them to be declared in order *)
let global_decls data scope =
  let report_redefinition name pos1 pos2 =
    let errstr = "Error: redefinition of \"" ^ name ^ "\": "
      ^ (format_position pos2) ^ ".\n"
      ^ "Original definition: " ^ (format_position pos1) ^ "."
    in raise (ProcessingError errstr)
  in
  let decl = function
    | DefFcn (pos, name, tp, _) ->
       begin match lookup_symbol_local scope.fcntable name with
       | None ->
          let llfnc =
            declare_function name (deftype2llvmtype scope tp) data.mdl
          in
          add_symbol scope.fcntable name (pos, tp, llfnc);
       | Some (oldpos, _, _) ->
          report_redefinition name oldpos pos
       end
    | _ -> ()
  in List.iter decl

let process_atom data scope = function
  | AtomInt (_, i) ->
     const_int (the (lookup_symbol scope.typemap "i32")) i
  | AtomVar (pos, v) ->
     begin
       match lookup_symbol scope.vartable v with
       | None ->
          let errstr = "Undeclared variable \"" ^ v ^ "\" at "
            ^ (format_position pos) ^ "\n"
            ^ (show_source pos)
          in fatal_error errstr
       | Some (_, _, llvar) -> llvar
     end

let process_expr data scope =
  let llvm_operator = function
    (* FIXME: Should specify a proper name for intermediate values. *)
    | OperMult _ -> (build_mul, "mult")
    | OperDiv _ -> (build_sdiv, "sdiv")
    | OperPlus _ -> (build_add, "add")
    | OperMinus _ -> (build_sub, "sub")
    | OperLT _ -> (build_icmp Icmp.Slt, "lt")
    | OperLTE _ -> (build_icmp Icmp.Sle, "le")
    | OperGT _ -> (build_icmp Icmp.Sgt, "gt")
    | OperGTE _ -> (build_icmp Icmp.Sge, "ge")
    | OperEquals _ -> (build_icmp Icmp.Eq, "eq")
    | _ -> failwith "llvm_operator not fully implemented"
  in
  let rec expr_gen = function
    | ExprAtom atom -> process_atom data scope atom
    | ExprBinary (op, left, right) ->
       let e1 = expr_gen left
       and e2 = expr_gen right
       and (func, ident) = llvm_operator op in
       func e1 e2 ident data.bldr
    | _ -> failwith "expr_gen not fully implemented."
  in expr_gen

let rec process_stmt data scope bb = function
  | Block stmts ->
     List.fold_left (process_stmt data scope) bb stmts
  | Return e ->
     let ret = process_expr data scope e in
     let _ = build_ret ret data.bldr in
     bb
  | _ -> failwith "process_stmt not fully implemented."

let toplevel_stmt data scope = function
  | DefFcn (_, name, tp, body) ->
     let (_, _, llfcn) = the (lookup_symbol scope.fcntable name) in
     let bb = append_block data.ctx "entry" llfcn in
     let () = position_at_end bb data.bldr in
     (* Add parameters as variables. *)
     let deeper_scope = match tp with
       | FcnType (args, _) ->
          let ds = push_scope_data scope
          and llparams = params llfcn in
          begin
            List.iteri
              (fun i (pos, n, tp) ->
                add_symbol scope.vartable n (pos, tp, llparams.(i)))
              args;
            ds
          end
       | _ -> failwith "Internal error.  Function had non-function type."
     in
     let _ = process_stmt data deeper_scope bb body
     in ()
  | _ -> failwith "toplevel_stmt: not fully implemented."

(** process_ast -> outfile -> module_name -> Ast.stmts
    Generate LLVM IR code for the given module and dump it to the specified
    output file. *)
let process_ast outfile module_name stmts =
  let ctx  = global_context () in
  let mdl  = create_module ctx module_name in
  let bldr = builder ctx in
  let data = { ctx = ctx; mdl = mdl; bldr = bldr } in
  let fcntable = make_symtab ()
  and vartable = make_symtab ()
  and typemap = builtin_types ctx in
  let scope =
    { fcntable = fcntable;
      vartable = vartable;
      typemap  = typemap }
  in
  global_decls data scope stmts;
  List.iter (toplevel_stmt data scope) stmts;
  print_module outfile mdl
