open Llvm

external build_cmpxchg :
  llvalue -> llvalue -> llvalue -> AtomicOrdering.t ->
    AtomicOrdering.t -> string -> llbuilder -> llvalue
      = "llvm_build_cmpxchg_bytecode" "llvm_build_cmpxchg_native"
