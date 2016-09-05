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

type typecategory =
  | SignedInteger

(** name, kind, bit-width, llvm type *)
val map_builtin_types : (string
                         * typecategory
                         * int
                         * (Llvm.llcontext -> Llvm.lltype)) list
