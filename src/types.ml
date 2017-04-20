(* Copyright (C) 2017  DEFC Authors

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License
   as published by the Free Software Foundation; either version 2
   of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
   02110-1301, USA.
 *)

open Llvm
open Util

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
  | DefTypeFcn of deftype list * deftype * bool
  | DefTypePtr of deftype
  | DefTypeArray of deftype * int
  | DefTypeNullPtr
  | DefTypeNamedStruct of string
  | DefTypeLiteralStruct of deftype list * string list
  | DefTypeStaticStruct of deftype list

type primitive_kind =
  | KindInteger
  | KindFloat

(** Return whether the given integer type is signed. *)
let signed_p = function
  | DefTypePrimitive p ->
     begin match p with
     | PrimBool -> true
     | PrimI8 | PrimI16 | PrimI32 | PrimI64 -> true
     | PrimU8 | PrimU16 | PrimU32 | PrimU64 -> false
     | _ -> Report.err_internal __FILE__ __LINE__
                                "signed_p of non-integer type."
     end
  | _ -> Report.err_internal __FILE__ __LINE__
                             "Assumed something was a primitive."

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

(** name, type, llvm type constructor, C type *)
let map_builtin_types =
  [ ("void", DefTypeVoid, void_type, "void");
    ("bool", DefTypePrimitive PrimBool, i1_type, "char"); 
    ("char", DefTypePrimitive PrimI8, i8_type, "char");
    ("uchar", DefTypePrimitive PrimU8, i8_type, "unsigned char");
    ("i8",  DefTypePrimitive PrimI8,  i8_type, "char");
    ("u8",  DefTypePrimitive PrimU8,  i8_type, "unsigned char");
    ("i16", DefTypePrimitive PrimI16, i16_type, "short");
    ("u16", DefTypePrimitive PrimU16, i16_type, "unsigned short");
    ("i32", DefTypePrimitive PrimI32, i32_type, "int");
    ("u32", DefTypePrimitive PrimU32, i32_type, "unsigned int");
    ("i64", DefTypePrimitive PrimI64, i64_type, "long long");
    ("u64", DefTypePrimitive PrimU64, i64_type, "unsigned long long");
    ("f32", DefTypePrimitive PrimF32, float_type, "float");
    ("f64", DefTypePrimitive PrimF64, double_type, "double")
  ]

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

(** Return true iff the given type is an integer type. *)
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

(** Return true iff the type is a signed integer. *)
let is_sinteger_type = function
  | DefTypePrimitive prim ->
     begin match prim with
     | PrimBool
     | PrimI8
     | PrimI16
     | PrimI32
     | PrimI64 -> true
     | PrimU8
     | PrimU16
     | PrimU32
     | PrimU64
     | PrimF32 | PrimF64 -> false
     end
  | _ -> false

(** Return true iff the type is an unsigned integer. *)
let is_uinteger_type = function
  | DefTypePrimitive prim ->
     begin match prim with
     | PrimU8
     | PrimU16
     | PrimU32
     | PrimU64 -> true
     | PrimBool
     | PrimI8
     | PrimI16
     | PrimI32
     | PrimI64
     | PrimF32 | PrimF64 -> false
     end
  | _ -> false

(** Return true iff the type is a pointer. *)
let is_pointer_type = function
  | DefTypePtr _ -> true
  | _ -> false

let ptr_size = 8

(** Return the size of the given type in bytes. *)
let rec size_of typemap = function
  | DefTypeUnresolved _ ->
     Report.err_internal __FILE__ __LINE__
       "size_of called on an unresolved type."
  | DefTypeVoid ->
     Report.err_internal __FILE__ __LINE__
       "size_of called on a void type."
  | DefTypePrimitive p ->
     begin match p with
     | PrimBool | PrimI8 | PrimU8 -> 1
     | PrimI16 | PrimU16 -> 2
     | PrimI32 | PrimU32 -> 4
     | PrimI64 | PrimU64 -> 8
     | PrimF32 -> 4
     | PrimF64 -> 8
     end
  | DefTypeFcn _ -> ptr_size
  | DefTypePtr _ -> ptr_size
  | DefTypeArray (tp, n) -> n * (size_of typemap tp)
  | DefTypeNullPtr -> ptr_size
  | DefTypeNamedStruct nm -> size_of typemap (the (lookup_symbol typemap nm))
  | DefTypeLiteralStruct (members, _)
  | DefTypeStaticStruct members ->
     (* FIXME: Take aligment into account. *)
     List.fold_left (fun accum t -> accum + (size_of typemap t))
       0 members

let rec string_of_type = function
  | DefTypeUnresolved (_, nm) -> "<" ^ nm ^ ">"
  | DefTypeVoid -> "void"
  | DefTypePrimitive t -> primitive2string t
  | DefTypePtr t -> "*" ^ (string_of_type t)
  | DefTypeArray (tp, n) ->
     "[" ^ (string_of_int n) ^ "]" ^ (string_of_type tp)
  | DefTypeNullPtr -> "nil"
  | DefTypeNamedStruct nm -> "struct " ^ nm
  | DefTypeLiteralStruct (members, _) ->
     "<literal>{ "
     ^ (String.concat ", " (List.map string_of_type members))
     ^ " }"
  | DefTypeStaticStruct members ->
     "{ "
     ^ (String.concat ", " (List.map string_of_type members))
     ^ " }"
  | _ -> "other"
