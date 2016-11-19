type primitive =
  | PrimBool
  | PrimI8  | PrimU8
  | PrimI16 | PrimU16
  | PrimI32 | PrimU32
  | PrimI64 | PrimU64
  | PrimF32
  | PrimF64

type visibility =
  | VisLocal
  | VisExported of Lexing.position

type deftype =
  | DefTypeUnresolved of Lexing.position * string
  | DefTypeVoid
  | DefTypePrimitive of primitive
  | DefTypeFcn of deftype list * deftype
  | DefTypePtr of deftype
  | DefTypeNamedStruct of string
  | DefTypeLiteralStruct of deftype list * string list

(** Return the more general of the two primitive types. *)
val generalize_primitives : primitive -> primitive -> primitive

(** Compare two types for equality.  There is no ordering, so zero indicates
    the types are identical and non-zero indicates non-identical. *)
val compare : deftype -> deftype -> int

(** name, type, llvm type constructor *)
val map_builtin_types :
  (string * deftype * (Llvm.llcontext -> Llvm.lltype)) list

(** Convert a primitive type to its string representation. *)
val primitive2string : primitive -> string

(** Return true iff the given type is an integer type. *)
val is_integer_type : deftype -> bool
