open Llvm

type primitive =
  | PrimBool of Lexing.position * bool
  (* schar, uchar *)
  | PrimI16 of Lexing.position * int32 
  | PrimU16 of Lexing.position * int32
  | PrimI32 of Lexing.position * int32
  | PrimU32 of Lexing.position * int32
  | PrimI64 of Lexing.position * int64
  | PrimU64 of Lexing.position * int64
  (* floating point *)

type deftype =
  | DefTypePrimitive of primitive
  | DefTypeFcn of deftype list * deftype

type typecategory =
  | SignedInteger

let map_builtin_types =
  [ ("i32", SignedInteger, 32, i32_type);
    ("bool", SignedInteger, 1, i1_type) ]
