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
open Llvmext (* token_type, dwarf_type *)
open Util

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
  | DefTypeLiteralUnion of int * deftype list * string list
  | DefTypeVAList
  | DefTypeWildcard
  | DefTypeLLVMToken

type primitive_kind =
  | KindInteger
  | KindFloat

(** Return whether the given integer type is signed. *)
let signed_p = function
  | DefTypePrimitive (p, _) ->
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
  | DefTypePrimitive (p1, _), DefTypePrimitive (p2, _) ->
     (* FIXME: Need to resolve qualifiers (volatile, const, etc.). *)
     compare_primitives p1 p2
  | DefTypeWildcard, _
  | _, DefTypeWildcard -> 0
  | _ -> failwith "Types.compare not fully implemented."

(** name, type, llvm type constructor, C type(s), bitwidth, dwarf type *)
let map_builtin_types =
  [ ("void", DefTypeVoid, void_type,
     ["void"], 0, DW_INVALID);
    ("bool", DefTypePrimitive (PrimBool, []), i1_type,
     ["char"], 1, DW_ATE_BOOLEAN);
    ("char", DefTypePrimitive (PrimI8, []), i8_type,
     ["char"],
     8, DW_ATE_SIGNED_CHAR);
    ("uchar", DefTypePrimitive (PrimU8, []), i8_type,
     ["unsigned char"], 8, DW_ATE_UNSIGNED_CHAR);
    ("i8",  DefTypePrimitive (PrimI8, []),  i8_type,
     ["char"; "signed char"], 8, DW_ATE_SIGNED_CHAR);
    ("u8",  DefTypePrimitive (PrimU8, []),  i8_type,
     ["unsigned char"], 8, DW_ATE_UNSIGNED_CHAR);
    ("i16", DefTypePrimitive (PrimI16, []), i16_type,
     ["short"; "signed short"], 16, DW_ATE_SIGNED);
    ("u16", DefTypePrimitive (PrimU16, []), i16_type,
     ["unsigned short"], 16, DW_ATE_UNSIGNED);
    ("i32", DefTypePrimitive (PrimI32, []), i32_type,
     ["int"; "signed int"], 32, DW_ATE_SIGNED);
    ("u32", DefTypePrimitive (PrimU32, []), i32_type,
     ["unsigned int"], 32, DW_ATE_UNSIGNED);
    ("i64", DefTypePrimitive (PrimI64, []), i64_type,
     ["long long"; "signed long long"; "long"; "signed long"],
     64, DW_ATE_SIGNED);
    ("u64", DefTypePrimitive (PrimU64, []), i64_type,
     ["unsigned long long"; "unsigned long"], 64, DW_ATE_UNSIGNED);
    ("f32", DefTypePrimitive (PrimF32, []), float_type,
     ["float"], 64, DW_ATE_FLOAT);
    ("f64", DefTypePrimitive (PrimF64, []), double_type,
     ["double"], 64, DW_ATE_FLOAT);
    ("llvm.token", DefTypeLLVMToken, token_type,
     [], 0, DW_INVALID)
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
  | DefTypePrimitive (prim, _) ->
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
  | DefTypePrimitive (prim, _) ->
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
  | DefTypePrimitive (prim, _) ->
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
  | DefTypeOpaque (_, nm) ->
     Report.err_internal __FILE__ __LINE__
                         ("size_of called on opaque type: " ^ nm)
  | DefTypePrimitive (p, _) ->
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
  | DefTypeNamedUnion nm -> size_of typemap (the (lookup_symbol typemap nm))
  | DefTypeLiteralUnion (sz, _, _) ->
     sz
  | DefTypeVAList ->
     Report.err_internal __FILE__ __LINE__ "Can't get size of a va_list."
  | DefTypeWildcard ->
     Report.err_internal __FILE__ __LINE__ "Can't get the size of a wildcard."
  | DefTypeLLVMToken ->
     Report.err_internal __FILE__ __LINE__ "Shouldn't need size of LLVM token."

(** Convert the type into its string representation. *)
let rec string_of_type = function
  | DefTypeUnresolved (_, nm) -> "<" ^ nm ^ ">"
  | DefTypeVoid -> "void"
  | DefTypeOpaque (_, nm) -> nm
  | DefTypePrimitive (t, _) -> primitive2string t (* FIXME: qualifiers *)
  | DefTypePtr (t, _) -> "*" ^ (string_of_type t) (* FIXME: qualifiers *)
  | DefTypeArray (tp, n) ->
     "[" ^ (string_of_int n) ^ "]" ^ (string_of_type tp)
  | DefTypeNullPtr -> "nil"
  | DefTypeNamedStruct nm -> "struct " ^ nm
  | DefTypeLiteralStruct (members, _) ->
     "{ "
     ^ (String.concat ", " (List.map string_of_type members))
     ^ " }"
  | DefTypeStaticStruct members ->
     "<static>{ "
     ^ (String.concat ", " (List.map string_of_type members))
     ^ " }"
  | DefTypeWildcard -> "_"
  | _ -> "other"

(** Return true iff the type is volatile. *)
let dt_is_volatile = function
  | DefTypePrimitive (_, qualifiers) ->
     List.exists (fun q -> q = Volatile) qualifiers
  | _ -> false (* FIXME: Implement. *)

(** Return the most general of the list of types. *)
let most_general_type pos typemap =
  let rec get_literal_struct nm =
    match lookup_symbol typemap nm with
    | Some (DefTypeLiteralStruct _ as t) -> t
    | Some (DefTypeNamedStruct nm2) -> get_literal_struct nm2
    | None -> Report.err_internal __FILE__ __LINE__
                                  ("struct " ^ nm ^ "undefined.")
    | _ -> Report.err_internal __FILE__ __LINE__
                               ("struct " ^ nm ^ " was not a literal struct.")
  in
  let rec get_literal_union nm =
    match lookup_symbol typemap nm with
    | Some (DefTypeLiteralUnion _ as t) -> t
    | Some (DefTypeNamedUnion nm2) -> get_literal_union nm2
    | None -> Report.err_internal __FILE__ __LINE__
                                  ("union " ^ nm ^ "undefined.")
    | _ -> Report.err_internal __FILE__ __LINE__
                               ("union " ^ nm ^ " was not a literal union.")
  in
  let generalize_qualifiers q1 q2 =
    (* FIXME: Needs to be more sophisticated. *)
    if q1 = [] then q2
    else if q2 = [] then q1
    else q1
  in
  let rec generalize t1 t2 =
    let generalizing_error () =
      Report.err_generalizing_types pos (string_of_type t1) (string_of_type t2)
    in
    let reconcile_member_types tplist1 tplist2 =
      try List.map2 generalize tplist1 tplist2
      with _ -> generalizing_error ()
    in
    match t1, t2 with
    | DefTypeWildcard, _ -> t2
    | _, DefTypeWildcard -> t1
    | DefTypeVAList, DefTypeVAList -> t1
    | DefTypeVAList, _
    | _, DefTypeVAList ->
       Report.err_internal __FILE__ __LINE__ "generalizing va_list."
    | DefTypeUnresolved _, _ -> t2
    | _, DefTypeUnresolved _ -> t1
    | DefTypeVoid, _
    | _, DefTypeVoid ->
       Report.err_internal __FILE__ __LINE__ "Don't know what to do with void"
    | DefTypeOpaque (_, nm1), DefTypeOpaque (_, nm2) ->
       if nm1 = nm2 then t1
       else generalizing_error ()
    | DefTypeOpaque _, _
    | _, DefTypeOpaque _ ->
       generalizing_error ()
    | DefTypePrimitive (p1, q1), DefTypePrimitive (p2, q2) ->
       DefTypePrimitive (generalize_primitives p1 p2,
                         generalize_qualifiers q1 q2)
    | DefTypePrimitive _, _
    | _, DefTypePrimitive _ ->
       generalizing_error ()
    | DefTypeFcn (params1, ret1, var1), DefTypeFcn (params2, ret2, var2) ->
       if params1 = params2 && ret1 = ret2 && var1 = var2 then
         DefTypeFcn(params1, ret1, var1)
       else
         generalizing_error ()
    | DefTypeFcn _, _
    | _, DefTypeFcn _ ->
       generalizing_error ()
    | DefTypePtr _, DefTypePtr (DefTypeVoid, _) -> t1
    | DefTypePtr (DefTypeVoid, _), DefTypePtr _ -> t2
    | DefTypePtr (st1, q1), DefTypeArray (st2, _) ->
       DefTypePtr (generalize st1 st2, q1)
    | DefTypePtr (st1, q1), DefTypePtr (st2, q2) ->
       DefTypePtr (generalize st1 st2, generalize_qualifiers q1 q2)
    | DefTypeArray (st1, _), DefTypePtr (st2, q2) ->
       DefTypePtr (generalize st1 st2, q2)
    | DefTypePtr _, DefTypeNullPtr -> t1
    | DefTypeNullPtr, DefTypePtr _ -> t2
    | DefTypePtr _, _
    | _, DefTypePtr _ ->
       generalizing_error ()
    | DefTypeArray (st1, n), DefTypeArray (st2, m) ->
       let subtype = generalize st1 st2 in
       if n = m then DefTypeArray (subtype, n)
       else DefTypePtr (subtype, [])
    | DefTypeArray (st1, _), DefTypeNullPtr -> DefTypePtr (st1, [])
    | DefTypeNullPtr, DefTypeArray (st2, _) -> DefTypePtr (st2, [])
    | DefTypeArray _, _
    | _, DefTypeArray _ ->
       generalizing_error ()
    | DefTypeNullPtr, DefTypeNullPtr -> DefTypeNullPtr
    | DefTypeNullPtr, _
    | _, DefTypeNullPtr ->
       generalizing_error ()
    | DefTypeNamedStruct s1, DefTypeNamedStruct s2 ->
       if s1 = s2 then t1
       else generalizing_error ()
    | DefTypeNamedStruct s, DefTypeLiteralStruct (tplist1, _)
    | DefTypeLiteralStruct (tplist1, _), DefTypeNamedStruct s
    | DefTypeNamedStruct s, DefTypeStaticStruct tplist1
    | DefTypeStaticStruct tplist1, DefTypeNamedStruct s ->
       begin match get_literal_struct s with
       | DefTypeLiteralStruct (tplist2, _) ->
          let _ = reconcile_member_types tplist1 tplist2 in
          DefTypeNamedStruct s
       | _ ->
          Report.err_internal __FILE__ __LINE__
                              "Named struct was actually not a struct."
       end
    | DefTypeNamedStruct _, _
    | _, DefTypeNamedStruct _ ->
       generalizing_error ()
    | DefTypeLiteralStruct _, DefTypeLiteralStruct _ ->
       if t1 = t2 then t1
       else generalizing_error ()
    | DefTypeLiteralStruct (tplist1, _), DefTypeStaticStruct tplist2 ->
       let reconciled = reconcile_member_types tplist1 tplist2 in
       DefTypeStaticStruct reconciled
    | DefTypeStaticStruct tplist1, DefTypeLiteralStruct (tplist2, _) ->
       let reconciled = reconcile_member_types tplist1 tplist2 in
       DefTypeStaticStruct reconciled
    | DefTypeLiteralStruct _, _
    | _, DefTypeLiteralStruct _ ->
       generalizing_error ()
    | DefTypeStaticStruct tplist1, DefTypeStaticStruct tplist2 ->
       DefTypeStaticStruct (reconcile_member_types tplist1 tplist2)
    | DefTypeStaticStruct _, _
    | _, DefTypeStaticStruct _ ->
       generalizing_error ()
    | DefTypeNamedUnion u, DefTypeNamedUnion v ->
       if u = v then t1 else generalizing_error ()
    | DefTypeNamedUnion u, DefTypeLiteralUnion (_, tplist1, _)
    | DefTypeLiteralUnion (_, tplist1, _), DefTypeNamedUnion u ->
       begin match get_literal_union u with
       | DefTypeLiteralUnion (_, tplist2, _) ->
          let _ = reconcile_member_types tplist1 tplist2 in
          DefTypeNamedUnion u
       | _ ->
          Report.err_internal __FILE__ __LINE__
                              "Named union was actually not a union."
       end
    | DefTypeNamedUnion _, _ | _, DefTypeNamedUnion _ ->
       generalizing_error ()
    | DefTypeLiteralUnion _, DefTypeLiteralUnion _ ->
       if t1 = t2 then t1
       else generalizing_error ()
    | DefTypeLiteralUnion _, _ | _, DefTypeLiteralUnion _ ->
       generalizing_error ()
    | DefTypeLLVMToken, DefTypeLLVMToken ->
       (* Shouldn't happen, but won't catch it here. *)
       t1
  in
  let rec most_general = function
    | [] -> DefTypeUnresolved (Util.faux_pos, "<unknown array>")
    | [ t ] -> t
    | t1 :: t2 :: rest ->
       most_general ((generalize t1 t2) :: rest)
  in
  most_general

(** Return true iff the given type contains a wildcard. *)
let rec contains_wildcard = function
  | DefTypeWildcard -> true
  | DefTypeLiteralStruct (tps, _)
  | DefTypeStaticStruct tps -> List.exists contains_wildcard tps
  | _ -> false

(** Get the dwarf type of a primitive type. *)
let dwarf_of =
  let dwarf = Hashtbl.create 16 in
  List.iter (fun (_, p, _, _, sz, d) -> Hashtbl.add dwarf p (sz, d))
            map_builtin_types;
  Hashtbl.find dwarf
