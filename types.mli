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

(** name, kind, bit-width, llvm type *)
val map_builtin_types : (string
                         * typecategory
                         * int
                         * (Llvm.llcontext -> Llvm.lltype)) list
