type typecategory =
  | SignedInteger

(** name, kind, bit-width, llvm type *)
val map_builtin_types : (string
                         * typecategory
                         * int
                         * (Llvm.llcontext -> Llvm.lltype)) list
