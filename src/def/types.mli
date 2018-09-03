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

type baretype =
  | DefTypeUnresolved of string
  | DefTypeVoid
  | DefTypeNamed of string
  | DefTypeOpaque of string
  | DefTypePrimitive of primitive
  | DefTypeFcn of deftype list * deftype * bool
  | DefTypePtr of deftype
  | DefTypeArray of deftype * int
  | DefTypeNullPtr
  | DefTypeEnum of string list
  | DefTypeLiteralStruct of (*is_packed=*)bool * deftype list * string list
  | DefTypeStaticStruct of (*is_packed=*)bool * deftype list
  | DefTypeLiteralUnion of deftype list * string list
  | DefTypeVAList
  | DefTypeWildcard
  | DefTypeLLVMToken

and deftype =
  { dtpos      : Lexing.position option;
    bare       : baretype;
    dtvolatile : bool
  }

(** Make a deftype from a position (option) and a bare type. *)
val maketype : Lexing.position option -> baretype -> deftype

(** Make a pointer type. *)
val makeptr : deftype -> deftype

(** Return a volatile version of the given type. *)
val volatile_of : deftype -> deftype

(** Return whether the given integer type is signed. *)
val signed_p : deftype -> bool

(** Return the more general of the two primitive types. *)
val generalize_primitives : primitive -> primitive -> primitive

(** Compare two types for equality.  Return true iff the types are equal. *)
val equivalent_types : deftype -> deftype -> bool

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

(** Return true iff the type is an array. *)
val is_array_type : deftype -> bool

(** Return the size and alignment of the given type in bytes. *)
val size_and_align_of : deftype Util.symtab -> deftype -> int * int

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

(** If the type is a named type, dereference it using the typemap until a
    non-named type is reached. *)
val concrete_of : Lexing.position option -> deftype Util.symtab -> deftype
                  -> deftype

(** Replace array types in function parameters with pointers.  DEF, like C,
    treats arrays as pointers in function types. *)
val dearray_fcn : deftype -> deftype
