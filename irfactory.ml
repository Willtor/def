open Ast
open Llvm

type llvm_data =
  { ctx  : llcontext;
    mdl  : llmodule;
    bldr : llbuilder }

let process_ast outfile module_name (stmts : Ast.stmt list) =
  let ctx  = global_context () in
  let mdl  = create_module ctx module_name in
  let bldr = builder ctx in
  let data = { ctx = ctx; mdl = mdl; bldr = bldr } in
  print_module outfile mdl
