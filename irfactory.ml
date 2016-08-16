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
    typemap  : lltype symtab
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
  | _ -> failwith "process_atom not fully implemented."

let process_expr data scope =
  let llvm_operator = function
    | OperMult _ -> (build_mul, "mult")
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
     (* FIXME: Need to push a scope and add parameters. *)
     let _ = process_stmt data scope bb body
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
  let fcntable = make_symtab () in
  let typemap = builtin_types ctx in
  let scope = { fcntable = fcntable; typemap = typemap } in
  global_decls data scope stmts;
  List.iter (toplevel_stmt data scope) stmts;
  print_module outfile mdl
