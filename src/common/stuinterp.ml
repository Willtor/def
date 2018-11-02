(* Copyright (C) 2018  DEFC Authors

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

open Parsetree
open Util

let generalize_nums a b =
  match a, b with

  (* Bool *)
  | StuBool _, StuBool _ -> a, b
  | StuBool (pos, bool), StuChar _ ->
     StuChar (pos, Char.chr (if bool then 1 else 0)), b
  | StuChar _, StuBool (pos, bool) ->
     a, StuChar (pos, Char.chr (if bool then 1 else 0))
  | StuBool (pos, bool), StuUChar _ ->
     StuUChar (pos, Char.chr (if bool then 1 else 0)), b
  | StuUChar _, StuBool (pos, bool) ->
     a, StuUChar (pos, Char.chr (if bool then 1 else 0))
  | StuBool (pos, bool), StuInt16 _ ->
     StuInt16 (pos, if bool then 1l else 0l), b
  | StuInt16 _, StuBool (pos, bool) ->
     a, StuInt16 (pos, if bool then 1l else 0l)
  | StuBool (pos, bool), StuUInt16 _ ->
     StuUInt16 (pos, if bool then 1l else 0l), b
  | StuUInt16 _, StuBool (pos, bool) ->
     a, StuUInt16 (pos, if bool then 1l else 0l)
  | StuBool (pos, bool), StuInt32 _ ->
     StuInt32 (pos, if bool then 1l else 0l), b
  | StuInt32 _, StuBool (pos, bool) ->
     a, StuInt32 (pos, if bool then 1l else 0l)
  | StuBool (pos, bool), StuUInt32 _ ->
     StuUInt32 (pos, if bool then 1l else 0l), b
  | StuUInt32 _, StuBool (pos, bool) ->
     a, StuUInt32 (pos, if bool then 1l else 0l)
  | StuBool (pos, bool), StuInt64 _ ->
     StuInt64 (pos, if bool then 1L else 0L), b
  | StuInt64 _, StuBool (pos, bool) ->
     a, StuInt64 (pos, if bool then 1L else 0L)
  | StuBool (pos, bool), StuFloat32 _ ->
     StuFloat32 (pos, if bool then 1.0 else 0.0), b
  | StuFloat32 _, StuBool (pos, bool) ->
     a, StuFloat32 (pos, if bool then 1.0 else 0.0)
  | StuBool (pos, bool), StuFloat64 _ ->
     StuFloat64 (pos, if bool then 1.0 else 0.0), b
  | StuFloat64 _, StuBool (pos, bool) ->
     a, StuFloat64 (pos, if bool then 1.0 else 0.0)

  (* Char *)
  | StuChar _, StuChar _ -> a, b
  | StuChar (pos, c), StuInt16 _ ->
     StuInt16 (pos, Int32.of_int (Char.code c)), b
  | StuInt16 _, StuChar (pos, c) ->
     a, StuInt16 (pos, Int32.of_int (Char.code c))
  | StuChar (pos, c), StuUInt16 _ ->
     StuUInt16 (pos, Int32.of_int (Char.code c)), b
  | StuUInt16 _, StuChar (pos, c) ->
     a, StuUInt16 (pos, Int32.of_int (Char.code c))
  | StuChar (pos, c), StuInt32 _ ->
     StuInt32 (pos, Int32.of_int (Char.code c)), b
  | StuInt32 _, StuChar (pos, c) ->
     a, StuInt32 (pos, Int32.of_int (Char.code c))
  | StuChar (pos, c), StuUInt32 _ ->
     StuUInt32 (pos, Int32.of_int (Char.code c)), b
  | StuUInt32 _, StuChar (pos, c) ->
     a, StuUInt32 (pos, Int32.of_int (Char.code c))
  | StuChar (pos, c), StuInt64 _ ->
     StuInt64 (pos, Int64.of_int (Char.code c)), b
  | StuInt64 _, StuChar (pos, c) ->
     a, StuInt64 (pos, Int64.of_int (Char.code c))
  | StuChar (pos, c), StuUInt64 _ ->
     StuUInt64 (pos, Int64.of_int (Char.code c)), b
  | StuUInt64 _, StuChar (pos, c) ->
     a, StuUInt64 (pos, Int64.of_int (Char.code c))
  | StuChar (pos, c), StuFloat32 _ ->
     StuFloat32 (pos, float_of_int (Char.code c)), b
  | StuFloat32 _, StuChar (pos, c) ->
     a, StuFloat32 (pos, float_of_int (Char.code c))
  | StuChar (pos, c), StuFloat64 _ ->
     StuFloat64 (pos, float_of_int (Char.code c)), b
  | StuFloat64 _, StuChar (pos, c) ->
     a, StuFloat64 (pos, float_of_int (Char.code c))

  (* Int16 *)
  | StuInt16 _, StuInt16 _ -> a, b
  | StuInt16 (pos, n), StuUInt16 _ -> StuUInt16 (pos, n), b
  | StuUInt16 _, StuInt16 (pos, n) -> a, StuUInt16 (pos, n)
  | StuInt16 (pos, n), StuInt32 _ -> StuInt32 (pos, n), b
  | StuInt32 _, StuInt16 (pos, n) -> a, StuInt32 (pos, n)
  | StuInt16 (pos, n), StuUInt32 _ -> StuUInt32 (pos, n), b
  | StuUInt32 _, StuInt16 (pos, n) -> a, StuUInt32 (pos, n)
  | StuInt16 (pos, n), StuInt64 _ -> StuInt64 (pos, Int64.of_int32 n), b
  | StuInt64 _, StuInt16 (pos, n) -> a, StuInt64 (pos, Int64.of_int32 n)
  | StuInt16 (pos, n), StuUInt64 _ -> StuUInt64 (pos, Int64.of_int32 n), b
  | StuUInt64 _, StuInt16 (pos, n) -> a, StuUInt64 (pos, Int64.of_int32 n)
  | StuInt16 (pos, n), StuFloat32 _ -> StuFloat32 (pos, Int32.to_float n), b
  | StuFloat32 _, StuInt16 (pos, n) -> a, StuFloat32 (pos, Int32.to_float n)
  | StuInt16 (pos, n), StuFloat64 _ -> StuFloat64 (pos, Int32.to_float n), b
  | StuFloat64 _, StuInt16 (pos, n) -> a, StuFloat64 (pos, Int32.to_float n)

  (* UInt16 *)
  | StuUInt16 _, StuUInt16 _ -> a, b
  | StuInt32 _, StuUInt16 (pos, n) -> a, StuInt32 (pos, n)
  | StuUInt16 (pos, n), StuUInt32 _ -> StuUInt32 (pos, n), b
  | StuUInt32 _, StuUInt16 (pos, n) -> a, StuUInt32 (pos, n)
  | StuUInt16 (pos, n), StuInt64 _ -> StuInt64 (pos, Int64.of_int32 n), b
  | StuInt64 _, StuUInt16 (pos, n) -> a, StuInt64 (pos, Int64.of_int32 n)
  | StuUInt16 (pos, n), StuUInt64 _ -> StuUInt64 (pos, Int64.of_int32 n), b
  | StuUInt64 _, StuUInt16 (pos, n) -> a, StuUInt64 (pos, Int64.of_int32 n)
  | StuUInt16 (pos, n), StuFloat32 _ -> StuFloat32 (pos, Int32.to_float n), b
  | StuFloat32 _, StuUInt16 (pos, n) -> a, StuFloat32 (pos, Int32.to_float n)
  | StuUInt16 (pos, n), StuFloat64 _ -> StuFloat64 (pos, Int32.to_float n), b
  | StuFloat64 _, StuUInt16 (pos, n) -> a, StuFloat64 (pos, Int32.to_float n)

  (* Int32 *)
  | StuInt32 _, StuInt32 _ -> a, b
  | StuInt32 (pos, n), StuUInt32 _ -> StuUInt32 (pos, n), b
  | StuUInt32 _, StuInt32 (pos, n) -> a, StuUInt32 (pos, n)
  | StuInt32 (pos, n), StuInt64 _ -> StuInt64 (pos, Int64.of_int32 n), b
  | StuInt64 _, StuInt32 (pos, n) -> a, StuInt64 (pos, Int64.of_int32 n)
  | StuInt32 (pos, n), StuUInt64 _ -> StuUInt64 (pos, Int64.of_int32 n), b
  | StuUInt64 _, StuInt32 (pos, n) -> a, StuUInt64 (pos, Int64.of_int32 n)
  | StuInt32 (pos, n), StuFloat32 _ -> StuFloat32 (pos, Int32.to_float n), b
  | StuFloat32 _, StuInt32 (pos, n) -> a, StuFloat32 (pos, Int32.to_float n)
  | StuInt32 (pos, n), StuFloat64 _ -> StuFloat64 (pos, Int32.to_float n), b
  | StuFloat64 _, StuInt32 (pos, n) -> a, StuFloat64 (pos, Int32.to_float n)

  (* UInt32 *)
  | StuUInt32 _, StuUInt32 _ -> a, b
  | StuUInt32 (pos, n), StuInt64 _ -> StuInt64 (pos, Int64.of_int32 n), b
  | StuInt64 _, StuUInt32 (pos, n) -> a, StuInt64 (pos, Int64.of_int32 n)
  | StuUInt32 (pos, n), StuUInt64 _ -> StuUInt64 (pos, Int64.of_int32 n), b
  | StuUInt64 _, StuUInt32 (pos, n) -> a, StuUInt64 (pos, Int64.of_int32 n)
  | StuUInt32 (pos, n), StuFloat32 _ -> StuFloat32 (pos, Int32.to_float n), b
  | StuFloat32 _, StuUInt32 (pos, n) -> a, StuFloat32 (pos, Int32.to_float n)
  | StuUInt32 (pos, n), StuFloat64 _ -> StuFloat64 (pos, Int32.to_float n), b
  | StuFloat64 _, StuUInt32 (pos, n) -> a, StuFloat64 (pos, Int32.to_float n)

  (* Int64 *)
  | StuInt64 _, StuInt64 _ -> a, b
  | StuInt64 (pos, n), StuUInt64 _ -> StuUInt64 (pos, n), b
  | StuUInt64 _, StuInt64 (pos, n) -> a, StuUInt64 (pos, n)
  | StuInt64 (pos, n), StuFloat32 _ -> StuFloat32 (pos, Int64.to_float n), b
  | StuFloat32 _, StuInt64 (pos, n) -> a, StuFloat32 (pos, Int64.to_float n)
  | StuInt64 (pos, n), StuFloat64 _ -> StuFloat64 (pos, Int64.to_float n), b
  | StuFloat64 _, StuInt64 (pos, n) -> a, StuFloat64 (pos, Int64.to_float n)

  (* UInt64 *)
  | StuUInt64 _, StuUInt64 _ -> a, b
  | StuUInt64 (pos, n), StuFloat32 _ -> StuFloat32 (pos, Int64.to_float n), b
  | StuFloat32 _, StuUInt64 (pos, n) -> a, StuFloat32 (pos, Int64.to_float n)
  | StuUInt64 (pos, n), StuFloat64 _ -> StuFloat64 (pos, Int64.to_float n), b
  | StuFloat64 _, StuUInt64 (pos, n) -> a, StuFloat64 (pos, Int64.to_float n)

  (* Float32 *)
  | StuFloat32 _, StuFloat32 _ -> a, b
  | StuFloat32 (pos, n), StuFloat64 _ -> StuFloat64 (pos, n), b
  | StuFloat64 _, StuFloat32 (pos, n) -> a, StuFloat64 (pos, n)

  (* Float64 *)
  | StuFloat64 _, StuFloat64 _ -> a, b

  | _ -> Error.fatal_error "non-numerical type."

let add pos =
  let debind = function
    | StuBinding (BBStu stu) -> stu
    | stu -> stu
  in
  let add accum param =
    match debind accum, debind param with
    | StuInt32 (_, a), StuInt32 (_, b) -> StuInt32 (pos, Int32.add a b)
    | _ -> Error.fatal_error "adding non-num type."
  in
  List.fold_left add (StuInt32 (pos, 0l))

let stu_builtins =
  [ ("+", add) ]

(** Return the default set of bindings. *)
let bindings_create () =
  let bindings = make_symtab () in
  List.iter (fun (k, v) -> add_symbol bindings k (BBNative v)) stu_builtins;
  bindings

(** Interpret a STU expression and return the result. *)
let rec eval_stu bindings = function
  | StuSexpr (_, []) ->
     Error.fatal_error "empty s-expression."
  | StuSexpr (pos, sexpr) ->
     begin
       match eval_stu bindings (List.hd sexpr) with
       | StuBinding (BBNative native_f) ->
          native_f pos (List.tl (List.map (eval_stu bindings) sexpr))
       | StuBinding _ ->
          Error.fatal_error
            "Not implemented, yet."
       | _ ->
          Error.fatal_error
            "Need suitable error: tried to call a non-function"
     end
  | (StuInt32 _) as v -> v
  | StuIdent tok ->
     begin
       match lookup_symbol bindings tok.td_text with
       | Some binding -> StuBinding binding
       | None ->
          Error.fatal_error
            "FIXME: suitable error for 'unknown STU symbol.'"
     end
  | _ ->
     Error.fatal_error "Not implemented."
