open Llvm

type typecategory =
  | SignedInteger

let map_builtin_types =
  [ ("i32", SignedInteger, 32, i32_type);
    ("bool", SignedInteger, 1, i1_type) ]
