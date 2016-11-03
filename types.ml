open Llvm

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

type primitive_kind =
  | KindInteger
  | KindFloat

(** Return the more general of the two primitive types. *)
let generalize_primitives p1 p2 =
  let get_kind = function
    | PrimBool
    | PrimI8  | PrimU8
    | PrimI16 | PrimU16
    | PrimI32 | PrimU32
    | PrimI64 | PrimU64
      -> KindInteger
    | PrimF32 | PrimF64
      -> KindFloat
  in
  let is_signed = function
    | PrimBool -> true
    | PrimI8 | PrimI16 | PrimI32 | PrimI64 -> true
    | PrimU8 | PrimU16 | PrimU32 | PrimU64 -> false
    | _ -> Report.err_internal __FILE__ __LINE__ "is_signed of non-integer type."
  in
  let get_rank = function
    | PrimBool -> 1
    | PrimI8  | PrimU8  -> 8
    | PrimI16 | PrimU16 -> 16
    | PrimI32 | PrimU32 -> 32
    | PrimI64 | PrimU64 -> 64
    | PrimF32 -> 32
    | PrimF64 -> 64
  in
  if p1 == p2 then p1
  else match (get_kind p1), (get_kind p2) with
  | KindInteger, KindInteger ->
     let rank1 = get_rank p1
     and rank2 = get_rank p2 in
     begin match (is_signed p1), (is_signed p2) with
     | true, true | false, false ->
        if rank1 > rank2 then p1
        else p2
     | true, false ->
        if rank2 >= rank1 then p2
        else p1
     | false, true ->
        if rank1 >= rank2 then p1
        else p2
     end
  | KindFloat, KindInteger -> p1
  | KindInteger, KindFloat -> p2
  | KindFloat, KindFloat ->
     let r1, r2 = get_rank p1, get_rank p2 in
     if r1 > r2 then p1 else p2

(** Compare two types for equality.  There is no ordering, so zero indicates
    the types are identical and non-zero indicates non-identical. *)
let compare t1 t2 =
  let compare_primitives p1 p2 = if p1 == p2 then 0 else 1 in
  match t1, t2 with
  | DefTypePrimitive p1, DefTypePrimitive p2 ->
     compare_primitives p1 p2
  | _ -> failwith "Types.compare not fully implemented."

(** name, type, llvm type constructor *)
let map_builtin_types =
  [ ("void", DefTypeVoid, void_type);
    ("bool", DefTypePrimitive PrimBool, i1_type); 
    ("char", DefTypePrimitive PrimI8, i8_type);
    ("uchar", DefTypePrimitive PrimU8, i8_type);
    ("i8",  DefTypePrimitive PrimI8,  i8_type);
    ("u8",  DefTypePrimitive PrimU8,  i8_type);
    ("i16", DefTypePrimitive PrimI16, i16_type);
    ("u16", DefTypePrimitive PrimU16, i16_type);
    ("i32", DefTypePrimitive PrimI32, i32_type);
    ("u32", DefTypePrimitive PrimU32, i32_type);
    ("i64", DefTypePrimitive PrimI64, i64_type);
    ("u64", DefTypePrimitive PrimU64, i64_type);
    ("f32", DefTypePrimitive PrimF32, float_type);
    ("f64", DefTypePrimitive PrimF64, double_type) ]

(** Convert a primitive type to its string representation. *)
let primitive2string = function
  | PrimBool -> "bool"
  | PrimI8  -> "i8"
  | PrimU8  -> "u8"
  | PrimI16 -> "i16"
  | PrimU16 -> "u16"
  | PrimI32 -> "i32"
  | PrimU32 -> "u32"
  | PrimI64 -> "i64"
  | PrimU64 -> "u64"
  | PrimF32 -> "f32"
  | PrimF64 -> "f64"

let is_integer_type = function
  | DefTypePrimitive prim ->
     begin match prim with
     | PrimBool
     | PrimI8  | PrimU8
     | PrimI16 | PrimU16
     | PrimI32 | PrimU32
     | PrimI64 | PrimU64 -> true
     | PrimF32 | PrimF64 -> false
     end
  | _ -> false
