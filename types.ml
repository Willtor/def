open Llvm

type primitive =
  | PrimBool
  (* schar, uchar *)
  | PrimI16 | PrimU16
  | PrimI32 | PrimU32
  | PrimI64 | PrimU64
  (* floating point *)

type deftype =
  | DefTypePrimitive of primitive
  | DefTypeFcn of deftype list * deftype

type typecategory =
  | SignedInteger

let map_builtin_types =
  [ ("i32", SignedInteger, 32, i32_type);
    ("bool", SignedInteger, 1, i1_type) ]
