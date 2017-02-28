open Llvm
open Llvm_scalar_opts

let optimize_ir opt_level llvm_module =
  let pass_manager = PassManager.create_function llvm_module in
  if opt_level > 0 then
    begin
      add_instruction_combination pass_manager;
      add_reassociation pass_manager;
      add_gvn pass_manager;
      add_cfg_simplification pass_manager;
      ignore (PassManager.initialize pass_manager)
    end;
  pass_manager
