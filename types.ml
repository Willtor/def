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

type primitive_kind =
  | KindInteger

(** Return the more general of the two primitive types. *)
let generalize_primitives p1 p2 =
  let get_kind = function
    | PrimBool
    | PrimI16 | PrimU16
    | PrimI32 | PrimU32
    | PrimI64 | PrimU64
      -> KindInteger
  in
  let is_signed = function
    | PrimBool -> true
    | PrimI16 | PrimI32 | PrimI64 -> true
    | PrimU16 | PrimU32 | PrimU64 -> false
  in
  let get_rank = function
    | PrimBool -> 1
    | PrimI16 | PrimU16 -> 16
    | PrimI32 | PrimU32 -> 32
    | PrimI64 | PrimU64 -> 64
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

(** Compare two types for equality.  There is no ordering, so zero indicates
    the types are identical and non-zero indicates non-identical. *)
let compare t1 t2 =
  let compare_primitives p1 p2 = if p1 == p2 then 0 else 1 in
  match t1, t2 with
  | DefTypePrimitive p1, DefTypePrimitive p2 ->
     compare_primitives p1 p2
  | _ -> failwith "Types.compare not fully implemented."

(** name, type, llvm type constructor *)
let map_builtin_primitives =
  [ ("bool", PrimBool, i1_type);
    ("i16", PrimI16, i16_type);
    ("u16", PrimU16, i16_type);
    ("i32", PrimI32, i32_type);
    ("u32", PrimU32, i32_type);
    ("i64", PrimI64, i64_type);
    ("u64", PrimU64, i64_type) ]

