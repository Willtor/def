type qualifier =
  | Volatile

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
  | VisExternal

type deftype =
  | DefTypeUnresolved of Lexing.position * string
  | DefTypeVoid
  | DefTypeOpaque of Lexing.position * string
  | DefTypePrimitive of primitive * qualifier list
  | DefTypeFcn of deftype list * deftype * bool
  | DefTypePtr of deftype * qualifier list
  | DefTypeArray of deftype * int
  | DefTypeNullPtr
  | DefTypeNamedStruct of string
  | DefTypeLiteralStruct of deftype list * string list
  | DefTypeStaticStruct of deftype list
  | DefTypeNamedUnion of string
  | DefTypeLiteralUnion of deftype list * string list
  | DefTypeVAList
  | DefTypeWildcard
  | DefTypeLLVMToken

(** Return whether the given integer type is signed. *)
val signed_p : deftype -> bool

(** Return the more general of the two primitive types. *)
val generalize_primitives : primitive -> primitive -> primitive

(** Compare two types for equality.  There is no ordering, so zero indicates
    the types are identical and non-zero indicates non-identical. *)
val compare : deftype -> deftype -> int

(** name, type, llvm type constructor, C type *)
val map_builtin_types :
  (string * deftype * (Llvm.llcontext -> Llvm.lltype)
   * string list * int * Llvmext.dwarf_type) list

(** Convert a primitive type to its string representation. *)
val primitive2string : primitive -> string

(** Return true iff the given type is an integer type. *)
val is_integer_type : deftype -> bool

(** Return true iff the type is a signed integer. *)
val is_sinteger_type : deftype -> bool

(** Return true iff the type is an unsigned integer. *)
val is_uinteger_type : deftype -> bool

(** Return true iff the type is a pointer. *)
val is_pointer_type : deftype -> bool

(** Return the size of the given type in bytes. *)
val size_of : deftype Util.symtab -> deftype -> int

(** Convert the type into its string representation. *)
val string_of_type : deftype -> string

(** Return true iff the type is volatile. *)
val dt_is_volatile : deftype -> bool

(** Return the most general of the list of types. *)
val most_general_type : Lexing.position -> deftype Util.symtab
                        -> deftype list -> deftype

(** Return true iff the given type contains a wildcard. *)
val contains_wildcard : deftype -> bool

(** Get the dwarf type of a primitive type. *)
val dwarf_of : deftype -> (int * Llvmext.dwarf_type)
