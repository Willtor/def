open Llvm

type typecategory =
  | SignedInteger

let map_builtin_types =
  [ ("i32", SignedInteger, i32_type);
    ("bool", SignedInteger, i1_type) ]
