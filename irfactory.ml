open Ast
open Llvm
open Util

type llvm_data =
  { ctx  : llcontext;
    mdl  : llmodule;
    bldr : llbuilder }

(** Gather the global names and types.  For simpler mutual recursion, DEF
    does not require them to be declared in order *)
let global_decls fcntable =
  let decl = function
    | DefFcn (profile, _) ->
       let get_name = function
         | NamedFunction (_, name, _, _) -> name
       in
       add_symbol fcntable (get_name profile) profile
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
  global_decls fcntable stmts;
  print_module outfile mdl
