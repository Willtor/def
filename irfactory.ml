open Ast
open Llvm
open Util

exception ProcessingError of string

type llvm_data =
  { ctx  : llcontext;
    mdl  : llmodule;
    bldr : llbuilder }

(** Gather the global names and types.  For simpler mutual recursion, DEF
    does not require them to be declared in order *)
let global_decls data fcntable =
  let report_redefinition name pos1 pos2 =
    let errstr = "Error: redefinition of \"" ^ name ^ "\": "
      ^ (format_position pos2) ^ ".\n"
      ^ "Original definition: " ^ (format_position pos1) ^ "."
    in fatal_error errstr
  in
  let decl = function
    | DefFcn (pos, name, tp, _) ->
       begin match lookup_symbol_local fcntable name with
       | None ->
          begin
            add_symbol fcntable name (pos, tp);
          (* declare_function name (deftype2llvmtype profile) data.mdl*)
          end
       | Some (oldpos, _) ->
          report_redefinition name oldpos pos
       end
    | _ -> ()
  in List.iter decl

(** process_ast -> outfile -> module_name -> Ast.stmts
    Generate LLVM IR code for the given module and dump it to the specified
    output file. *)
let process_ast outfile module_name stmts =
  let ctx  = global_context () in
  let mdl  = create_module ctx module_name in
  let bldr = builder ctx in
  let data = { ctx = ctx; mdl = mdl; bldr = bldr } in
  let fcntable = make_symtab () in
  global_decls data fcntable stmts;
  print_module outfile mdl
