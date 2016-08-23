type typecategory =
  | SignedInteger

val map_builtin_types : (string
                         * typecategory
                         * (Llvm.llcontext -> Llvm.lltype)) list
