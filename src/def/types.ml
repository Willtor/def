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
  | DefTypeLiteralStruct of deftype list * string list
  | DefTypeStaticStruct of deftype list
  | DefTypeLiteralUnion of deftype list * string list
  | DefTypeVAList
  | DefTypeWildcard
  | DefTypeLLVMToken

and deftype =
  { dtpos      : Lexing.position option;
    bare       : baretype;
    dtvolatile : bool
  }

type primitive_kind =
  | KindInteger
  | KindFloat

(** Make a deftype from a position (option) and a bare type. *)
let maketype dtpos bare =
  { dtpos      = dtpos;
    bare       = bare;
    dtvolatile = false;
  }

(** Make a bare pointer type. *)
let makeptr tp = maketype None (DefTypePtr tp)

(** Return a volatile version of the given type. *)
let volatile_of tp =
  { dtpos      = tp.dtpos;
    bare       = tp.bare;
    dtvolatile = true
  }

(** Return whether the given integer type is signed. *)
let signed_p t = match t.bare with
  | DefTypePrimitive p ->
     begin match p with
     | PrimBool -> true
     | PrimI8 | PrimI16 | PrimI32 | PrimI64 -> true
     | PrimU8 | PrimU16 | PrimU32 | PrimU64 -> false
     | _ -> Report.err_internal __FILE__ __LINE__
                                "signed_p of non-integer type."
     end
  | DefTypeEnum _ -> false
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

(** Compare two types for equality.  Return true iff the types are equal. *)
let rec equivalent_types t1 t2 =
  let type_folder curr sub1 sub2 = curr && equivalent_types sub1 sub2 in
  match t1.bare, t2.bare with
  | DefTypePrimitive p1, DefTypePrimitive p2 ->
     (* FIXME: Need to resolve qualifiers (volatile, const, etc.). *)
     p1 = p2
  | DefTypeWildcard, _
  | _, DefTypeWildcard -> true
  | DefTypeFcn (p1, r1, v1), DefTypeFcn (p2, r2, v2) when v1 = v2 ->
     begin
       try
         (List.fold_left2 type_folder true p1 p2)
         && equivalent_types r1 r2
       with _ -> false
     end
  | DefTypePtr sub1, DefTypePtr sub2
  | DefTypeArray (sub1, _), DefTypeArray (sub2, _) ->
     equivalent_types sub1 sub2
  | DefTypeLiteralStruct (sub1, nm1), DefTypeLiteralStruct (sub2, nm2)
  | DefTypeLiteralUnion (sub1, nm1), DefTypeLiteralUnion (sub2, nm2) ->
     begin
       try
         (List.fold_left2 type_folder true sub1 sub2)
         && nm1 = nm2
       with _ -> false
     end
  | b1, b2 -> b1 = b2

(** name, type, llvm type constructor, C type(s), bitwidth, dwarf type *)
let map_builtin_types =
  let makebare = maketype None in
  [ ("void", makebare DefTypeVoid, void_type,
     ["void"], 0, DW_INVALID);
    ("bool", makebare (DefTypePrimitive PrimBool), i1_type,
     ["char"], 1, DW_ATE_BOOLEAN);
    ("char", makebare (DefTypePrimitive PrimI8), i8_type,
     ["char"],
     8, DW_ATE_SIGNED_CHAR);
    ("uchar", makebare (DefTypePrimitive PrimU8), i8_type,
     ["unsigned char"], 8, DW_ATE_UNSIGNED_CHAR);
    ("i8", makebare (DefTypePrimitive PrimI8),  i8_type,
     ["char"; "signed char"], 8, DW_ATE_SIGNED_CHAR);
    ("u8", makebare (DefTypePrimitive PrimU8),  i8_type,
     ["unsigned char"], 8, DW_ATE_UNSIGNED_CHAR);
    ("i16", makebare (DefTypePrimitive PrimI16), i16_type,
     ["short"; "signed short"], 16, DW_ATE_SIGNED);
    ("u16", makebare (DefTypePrimitive PrimU16), i16_type,
     ["unsigned short"], 16, DW_ATE_UNSIGNED);
    ("i32", makebare (DefTypePrimitive PrimI32), i32_type,
     ["int"; "signed int"], 32, DW_ATE_SIGNED);
    ("u32", makebare (DefTypePrimitive PrimU32), i32_type,
     ["unsigned int"], 32, DW_ATE_UNSIGNED);
    ("i64", makebare (DefTypePrimitive PrimI64), i64_type,
     ["long long"; "signed long long"; "long"; "signed long"],
     64, DW_ATE_SIGNED);
    ("u64", makebare (DefTypePrimitive PrimU64), i64_type,
     ["unsigned long long"; "unsigned long"], 64, DW_ATE_UNSIGNED);
    ("f32", makebare (DefTypePrimitive PrimF32), float_type,
     ["float"], 64, DW_ATE_FLOAT);
    ("f64", makebare (DefTypePrimitive PrimF64), double_type,
     ["double"; "long double"], 64, DW_ATE_FLOAT);
    ("llvm.token", makebare DefTypeLLVMToken, token_type,
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
let is_integer_type t = match t.bare with
  | DefTypePrimitive prim ->
     begin match prim with
     | PrimBool
     | PrimI8  | PrimU8
     | PrimI16 | PrimU16
     | PrimI32 | PrimU32
     | PrimI64 | PrimU64 -> true
     | PrimF32 | PrimF64 -> false
     end
  | DefTypeEnum _ -> true
  | _ -> false

(** Return true iff the type is a signed integer. *)
let is_sinteger_type t = match t.bare with
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
let is_uinteger_type t = match t.bare with
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
let is_pointer_type t = match t.bare with
  | DefTypePtr _ -> true
  | _ -> false

(** Return true iff the type is an array. *)
let is_array_type t = match t.bare with
  | DefTypeArray _ -> true
  | _ -> false

let ptr_size = 8

(** Return the size of the given type in bytes. *)
let size_of typemap tp =
  let rec size_n_alignment t =
    match t.bare with
    | DefTypeUnresolved _ ->
       Report.err_internal __FILE__ __LINE__
                           "size_of called on an unresolved type."
    | DefTypeVoid ->
       Report.err_internal __FILE__ __LINE__
                           "size_of called on a void type."
    | DefTypeNamed nm ->
       size_n_alignment (the @@ lookup_symbol typemap nm)
    | DefTypeOpaque nm ->
       Report.err_internal __FILE__ __LINE__
                           ("size_of called on opaque type: " ^ nm)
    | DefTypePrimitive p ->
       let sz = match p with
         | PrimBool | PrimI8 | PrimU8 -> 1
         | PrimI16 | PrimU16 -> 2
         | PrimI32 | PrimU32 -> 4
         | PrimI64 | PrimU64 -> 8
         | PrimF32 -> 4
         | PrimF64 -> 8
       in
       sz, sz
    | DefTypeFcn _ -> ptr_size, ptr_size
    | DefTypePtr _ -> ptr_size, ptr_size
    | DefTypeArray (tp, n) ->
       let sz, align = size_n_alignment tp in
       n * sz, align
    | DefTypeNullPtr -> ptr_size, ptr_size
    | DefTypeEnum _ ->
       (* FIXME: Clang always seems to make enums 4 bytes, even if they could
          be smaller.  Need to investigate this further. *)
       4, 4
    | DefTypeLiteralStruct (members, _)
    | DefTypeStaticStruct members ->
       let member_proc (accum_sz, accum_align) member =
         let sz, align = size_n_alignment member in
         let depth_into_alignment = accum_sz mod align in
         let aligned_sz = if depth_into_alignment = 0 then accum_sz
                          else accum_sz + (align - depth_into_alignment)
         in
         aligned_sz + sz, max align accum_align
       in
       let accum_sz, align = List.fold_left member_proc (0, 0) members in
       let depth_into_alignment = accum_sz mod align in
       let padded_sz = if depth_into_alignment = 0 then accum_sz
                       else accum_sz + (align - depth_into_alignment)
       in
       padded_sz, align
    | DefTypeLiteralUnion (members, _) ->
       let select_max (curr_sz, curr_align) m =
         let sz, align = size_n_alignment m in
         max sz curr_sz, max align curr_align
       in
       List.fold_left select_max (0, 0) members
    | DefTypeVAList ->
       Report.err_internal __FILE__ __LINE__ "Can't get size of a va_list."
    | DefTypeWildcard ->
       Report.err_internal __FILE__ __LINE__ "Can't get the size of a wildcard."
    | DefTypeLLVMToken ->
       Report.err_internal __FILE__ __LINE__ "Shouldn't need size of LLVM token."
  in
  let sz, _ = size_n_alignment tp in sz

(** Convert the type into its string representation. *)
let rec string_of_type t = match t.bare with
  | DefTypeUnresolved nm -> "<" ^ nm ^ ">"
  | DefTypeVoid -> "void"
  | DefTypeNamed nm -> "<named> " ^ nm
  | DefTypeOpaque nm -> "<opaque> " ^ nm
  | DefTypePrimitive t -> primitive2string t
  | DefTypeFcn (params, ret, is_va) ->
     "(" ^ (String.concat ", " (List.map string_of_type params))
     ^ (if is_va then ", ...) -> " else ") -> ")
     ^ string_of_type ret
  | DefTypePtr t ->
     "*" ^ (string_of_type t)
  | DefTypeArray (tp, n) ->
     "[" ^ (string_of_int n) ^ "]" ^ (string_of_type tp)
  | DefTypeNullPtr -> "nil"
  | DefTypeEnum variants -> "enum | " ^ (String.concat " | " variants)
  | DefTypeLiteralStruct (members, _) ->
     "{ "
     ^ (String.concat ", " (List.map string_of_type members))
     ^ " }"
  | DefTypeStaticStruct members ->
     "<static>{ "
     ^ (String.concat ", " (List.map string_of_type members))
     ^ " }"
  | DefTypeLiteralUnion (members, _) ->
     "union { "
     ^ (String.concat ", " (List.map string_of_type members))
     ^ " }"
  | DefTypeWildcard -> "_"
  | _ -> "other"

(** Return true iff the type is volatile. *)
let dt_is_volatile t = t.dtvolatile

(** Return the most general of the list of types. *)
let most_general_type pos typemap =
  let rec generalize t1 t2 =
    let generalizing_error () =
      Report.err_generalizing_types pos (string_of_type t1) (string_of_type t2)
    in
    let reconcile_member_types tplist1 tplist2 =
      try List.map2 generalize tplist1 tplist2
      with _ -> generalizing_error ()
    in
    match t1.bare, t2.bare with
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
    | DefTypeNamed nm1, DefTypeNamed nm2 ->
       if nm1 = nm2 then t1
       else generalizing_error ()
    | DefTypeNamed nm, _ ->
       generalize (Util.the @@ lookup_symbol typemap nm) t2
    | _, DefTypeNamed nm ->
       generalize t1 (Util.the @@ lookup_symbol typemap nm)
    | DefTypeOpaque nm1, DefTypeOpaque nm2 ->
       if nm1 = nm2 then t1
       else generalizing_error ()
    | DefTypeOpaque _, _
    | _, DefTypeOpaque _ ->
       generalizing_error ()
    | DefTypePrimitive p1, DefTypePrimitive p2 ->
       let bare = DefTypePrimitive (generalize_primitives p1 p2)
       in maketype None bare
    | DefTypePrimitive _, _
    | _, DefTypePrimitive _ ->
       generalizing_error ()
    | DefTypeFcn (params1, ret1, var1), DefTypeFcn (params2, ret2, var2) ->
       if params1 = params2 && ret1 = ret2 && var1 = var2 then
         let bare = DefTypeFcn(params1, ret1, var1) in
         maketype None bare
       else
         generalizing_error ()
    | DefTypeFcn _, _
    | _, DefTypeFcn _ ->
       generalizing_error ()
    | DefTypePtr _, DefTypePtr ({ bare = DefTypeVoid }) -> t1
    | DefTypePtr ({ bare = DefTypeVoid }), DefTypePtr _ -> t2
    | DefTypePtr st1, DefTypeArray (st2, _) ->
       let bare = DefTypePtr (generalize st1 st2) in
       maketype None bare
    | DefTypePtr st1, DefTypePtr st2 ->
       let bare = DefTypePtr (generalize st1 st2)
       in maketype None bare
    | DefTypeArray (st1, _), DefTypePtr st2 ->
       let bare = DefTypePtr (generalize st1 st2) in
       maketype None bare
    | DefTypePtr _, DefTypeNullPtr -> t1
    | DefTypeNullPtr, DefTypePtr _ -> t2
    | DefTypePtr _, _
    | _, DefTypePtr _ ->
       generalizing_error ()
    | DefTypeEnum vlist1, DefTypeEnum vlist2 ->
       if t1 = t2 then t1 else generalizing_error ()
    | DefTypeEnum _, _
    | _, DefTypeEnum _ ->
       generalizing_error ()
    | DefTypeArray (st1, n), DefTypeArray (st2, m) ->
       let subtype = generalize st1 st2 in
       let bare = if n = m then DefTypeArray (subtype, n)
                  else DefTypePtr subtype
       in
       maketype None bare
    | DefTypeArray (st1, _), DefTypeNullPtr ->
       let bare = DefTypePtr st1 in
       maketype None bare
    | DefTypeNullPtr, DefTypeArray (st2, _) ->
       let bare = DefTypePtr st2 in
       maketype None bare
    | DefTypeArray _, _
    | _, DefTypeArray _ ->
       generalizing_error ()
    | DefTypeNullPtr, DefTypeNullPtr -> t1
    | DefTypeNullPtr, _
    | _, DefTypeNullPtr ->
       generalizing_error ()
    | DefTypeLiteralStruct _, DefTypeLiteralStruct _ ->
       if t1 = t2 then t1
       else generalizing_error ()
    | DefTypeLiteralStruct (tplist1, _), DefTypeStaticStruct tplist2 ->
       let reconciled = reconcile_member_types tplist1 tplist2 in
       let bare = DefTypeStaticStruct reconciled in
       maketype None bare
    | DefTypeStaticStruct tplist1, DefTypeLiteralStruct (tplist2, _) ->
       let reconciled = reconcile_member_types tplist1 tplist2 in
       let bare = DefTypeStaticStruct reconciled in
       maketype None bare
    | DefTypeLiteralStruct _, _
    | _, DefTypeLiteralStruct _ ->
       generalizing_error ()
    | DefTypeStaticStruct tplist1, DefTypeStaticStruct tplist2 ->
       let bare = DefTypeStaticStruct (reconcile_member_types tplist1 tplist2)
       in maketype None bare
    | DefTypeStaticStruct _, _
    | _, DefTypeStaticStruct _ ->
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
    | [] ->
       let bare = DefTypeUnresolved "<unknown array>" in
       maketype None bare
    | [ t ] -> t
    | t1 :: t2 :: rest ->
       most_general ((generalize t1 t2) :: rest)
  in
  most_general

(** Return true iff the given type contains a wildcard. *)
let rec contains_wildcard t = match t.bare with
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

(** If the type is a named type, dereference it using the typemap until a
    non-named type is reached. *)
let concrete_of typemap tp =
  let rec concrete tp =
    match tp.bare with
    | DefTypeNamed name ->
       concrete (Util.the @@ lookup_symbol typemap name)
    | _ -> tp
  in
  concrete tp
