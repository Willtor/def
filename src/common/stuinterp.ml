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

type op_kind =
  | Multi of (int32 -> int32 -> int32) * (int64 -> int64 -> int64)
             * (float -> float -> float)
  | Binary of (int32 -> int32 -> int32) * (int64 -> int64 -> int64)
              * (float -> float -> float)
  | Unary of (int32 -> int32) * (int64 -> int64) * (float -> float)

let is_num stu =
  match stu with
  | StuChar _ | StuUChar _
  | StuInt16 _ | StuUInt16 _
  | StuInt32 _ | StuUInt32 _
  | StuInt64 _ | StuUInt64 _
  | StuFloat32 _ | StuFloat64 _ -> stu
  | _ -> Error.fatal_error "STU value isn't a number."

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

let debind = function
  | StuBinding (BBStu stu) -> stu
  | stu -> stu

let mathop name op_kind =
  let char i32 a b =
    let rawval =
      i32 (Int32.of_int (Char.code a)) (Int32.of_int (Char.code b))
    in
    Char.chr ((Int32.to_int rawval) mod 256)
  in
  fun pos args ->
  let binop i32 i64 float accum value =
    match generalize_nums (debind accum) (debind value) with
    | StuChar (pos, n), StuChar (_, m) -> StuChar (pos, char i32 n m)
    | StuUChar (pos, n), StuUChar (_, m) -> StuUChar (pos, char i32 n m)
    | StuInt16 (pos, n), StuInt16 (_, m) -> StuInt16 (pos, i32 n m)
    | StuUInt16 (pos, n), StuUInt16 (_, m) -> StuUInt16 (pos, i32 n m)
    | StuInt32 (pos, n), StuInt32 (_, m) -> StuInt32 (pos, i32 n m)
    | StuUInt32 (pos, n), StuUInt32 (_, m) -> StuUInt32 (pos, i32 n m)
    | StuInt64 (pos, n), StuInt64 (_, m) -> StuInt64 (pos, i64 n m)
    | StuUInt64 (pos, n), StuUInt64 (_, m) -> StuUInt64 (pos, i64 n m)
    | StuFloat32 (pos, n), StuFloat32 (_, m) -> StuFloat32 (pos, float n m)
    | StuFloat64 (pos, n), StuFloat64 (_, m) -> StuFloat64 (pos, float n m)
    | _ -> Error.fatal_error "Internal error in a STU mathop."
  in
  if args = [] then
    Error.fatal_error ("need suitable error for noargs STU " ^ name)
  else
    match op_kind with
    | Multi (i32, i64, float) ->
       let first = is_num (debind (List.hd args)) in
       List.fold_left (binop i32 i64 float) first (List.tl args)
    | Binary (i32, i64, float) ->
       if List.length args <> 2 then
         Error.fatal_error "require two arguments."
       else
         let first = is_num (debind (List.hd args)) in
         let second = is_num (debind (List.hd (List.tl args))) in
         binop i32 i64 float first second
    | Unary _ ->
       Error.fatal_error "unary not implemented, yet."

let add = mathop "+" @@ Multi (Int32.add, Int64.add, (+.))
let sub = mathop "-" @@ Multi (Int32.sub, Int64.sub, (-.))
let mul = mathop "*" @@ Multi (Int32.mul, Int64.mul, ( *. ))
let div = mathop "/" @@ Multi (Int32.div, Int64.div, (/.))
let modulo = mathop "%" @@
               Binary (Int32.rem, Int64.rem,
                       (fun _ -> Error.fatal_error
                                   "Modulo doesn't apply to floats."))

let generic_conv pos bool char i32 i64 float = function
  | [StuBool (p, v)] -> p, bool v
  | [StuChar (p, v)]    | [StuUChar (p, v)] -> p, char v
  | [StuInt16 (p, v)]   | [StuUInt16 (p, v)] -> p, i32 v
  | [StuInt32 (p, v)]   | [StuUInt32 (p, v)] -> p, i32 v
  | [StuInt64 (p, v)]   | [StuUInt64 (p, v)] -> p, i64 v
  | [StuFloat32 (p, v)] | [StuFloat64 (p, v)] -> p, float v
  | [] -> Error.fatal_error "need suitable err for conv w/ no args."
  | _ -> Error.fatal_error "need suitable err for bad conv."

let ident x = x

let bool_conv pos =
  generic_conv pos
    ident
    (fun c -> if (Char.code c) = 0 then false else true)
    (fun n -> if Int32.equal n 0l then false else true)
    (fun n -> if Int64.equal n 0L then false else true)
    (fun n -> if n <> 0.0 then true else false)

let cchr n =
  if n > 255 then Char.chr 255
  else if n < 0 then Char.chr 0
  else Char.chr n

let char_conv pos =
  generic_conv pos
    (fun b -> if b then Char.chr 1 else Char.chr 0)
    ident
    (fun n -> cchr (Int32.to_int n))
    (fun n -> cchr (Int64.to_int n))
    (fun n -> cchr (int_of_float n))

let i32_conv pos =
  generic_conv pos
    (fun b -> if b then 1l else 0l)
    (fun c -> Int32.of_int (Char.code c))
    ident
    Int64.to_int32
    Int32.of_float

let i64_conv pos =
  generic_conv pos
    (fun b -> if b then 1L else 0L)
    (fun c -> Int64.of_int (Char.code c))
    Int64.of_int32
    ident
    Int64.of_float

let float_conv pos =
  generic_conv pos
    (fun b -> if b then 1.0 else 0.0)
    (fun c -> float_of_int (Char.code c))
    Int32.to_float
    Int64.to_float
    ident

let stu_builtins =
  [ (*-- Operations --*)
    ("+", add); ("-", sub);
    ("*", mul); ("/", div);
    ("%", modulo);

    (*-- Conversions --*)
    ("bool",
     (fun pos args -> let p, v = bool_conv pos args in StuBool (p, v)));
    ("char",
     (fun pos args -> let p, v = char_conv pos args in StuChar (p, v)));
    ("uchar",
     (fun pos args -> let p, v = char_conv pos args in StuUChar (p, v)));
    ("i16",
     (fun pos args -> let p, v = i32_conv pos args in StuInt16 (p, v)));
    ("u16",
     (fun pos args -> let p, v = i32_conv pos args in StuUInt16 (p, v)));
    ("i32",
     (fun pos args -> let p, v = i32_conv pos args in StuInt32 (p, v)));
    ("u32",
     (fun pos args -> let p, v = i32_conv pos args in StuUInt32 (p, v)));
    ("i64",
     (fun pos args -> let p, v = i64_conv pos args in StuInt64 (p, v)));
    ("u64",
     (fun pos args -> let p, v = i64_conv pos args in StuUInt64 (p, v)));
    ("float32",
     (fun pos args -> let p, v = float_conv pos args in StuFloat32 (p, v)));
    ("float64",
     (fun pos args -> let p, v = float_conv pos args in StuFloat64 (p, v)));
  ]

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
  | (StuBool _) as v -> v
  | (StuChar _) as v -> v
  | (StuUChar _) as v -> v
  | (StuInt16 _) as v -> v
  | (StuUInt16 _) as v -> v
  | (StuInt32 _) as v -> v
  | (StuUInt32 _) as v -> v
  | (StuInt64 _) as v -> v
  | (StuUInt64 _) as v -> v
  | (StuFloat32 _) as v -> v
  | (StuFloat64 _) as v -> v
  | StuIdent tok ->
     begin
       match lookup_symbol bindings tok.td_text with
       | Some binding -> StuBinding binding
       | None ->
          Error.fatal_error
            ("FIXME: suitable error for unknown sym: " ^ tok.td_text)
     end
  | _ ->
     Error.fatal_error "Not implemented."
