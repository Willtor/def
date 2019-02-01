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

exception ExNoFloatPermitted

type op_kind =
  | Multi of (int32 -> int32 -> int32) * (int64 -> int64 -> int64)
             * (float -> float -> float)
  | Binary of (int32 -> int32 -> int32) * (int64 -> int64 -> int64)
              * (float -> float -> float)
  | Unary of (int32 -> int32) * (int64 -> int64) * (float -> float)

(** Return the length of an S-expression.  If the arg isn't an S-expr, an
    error is generated. *)
let sexpr_length =
  let rec count_nodes n = function
    | IsmNode (_, next) -> count_nodes (n + 1) next
    | IsmTerm _ -> n
    | ism ->
       Ismerr.err_malformed_sexpr (pos_of_ism ism)
  in
  count_nodes 0

(** From the given fcn-name's parameters return the single argument (or
    report an error otherwise). *)
let ism_extract_single_arg fcn = function
  | IsmNode (arg, IsmTerm _) -> arg
  | args ->
     Ismerr.err_args_mismatch (pos_of_ism args) 1 (sexpr_length args)

(** From the given fcn-name's parameters return two arguments (or
    report an error otherwise). *)
let ism_extract_two_args fcn = function
  | IsmNode (arg1, IsmNode (arg2, IsmTerm _)) -> arg1, arg2
  | args ->
     Ismerr.err_args_mismatch (pos_of_ism args) 2 (sexpr_length args)

let split_list = function
  | IsmNode (obj, rest) -> obj, rest
  | _ ->
     Ismerr.internal "Tried to split non-list."

let extract_int64 = function
  | IsmBool (_, true) -> 1L
  | IsmBool (_, false) -> 0L
  | IsmChar (_, n) | IsmUChar (_, n) ->
     Int64.of_int (Char.code n)
  | IsmInt16 (_, n) | IsmUInt16 (_, n)
  | IsmInt32 (_, n) | IsmUInt32 (_, n) ->
     Int64.of_int32 n
  | IsmInt64 (_, n) | IsmUInt64 (_, n) ->
     n
  | ism ->
     Ismerr.err_non_int (pos_of_ism ism)

let iter f args =
  let rec it = function
    | IsmTerm _ -> ()
    | IsmNode (obj, rest) ->
       (f obj; it rest)
    | _ ->
       Ismerr.internal "FIXME: Need suitable error."
  in
  it args

let fold_left f start args =
  let rec fold accum = function
    | IsmTerm _ -> accum
    | IsmNode (obj, rest) ->
       fold (f accum obj) rest
    | _ ->
       Ismerr.internal "FIXME: Need suitable error."
  in
  fold start args

let is_num ism =
  match ism with
  | IsmChar _ | IsmUChar _
  | IsmInt16 _ | IsmUInt16 _
  | IsmInt32 _ | IsmUInt32 _
  | IsmInt64 _ | IsmUInt64 _
  | IsmFloat32 _ | IsmFloat64 _ -> ism
  | IsmBinding _ -> Ismerr.internal "is_num found a binding."
  | _ ->
     Ismerr.err_nan (pos_of_ism ism)

let generalize_nums a b =
  match a, b with

  (* Bool *)
  | IsmBool _, IsmBool _ -> a, b
  | IsmBool (pos, bool), IsmChar _ ->
     IsmChar (pos, Char.chr (if bool then 1 else 0)), b
  | IsmChar _, IsmBool (pos, bool) ->
     a, IsmChar (pos, Char.chr (if bool then 1 else 0))
  | IsmBool (pos, bool), IsmUChar _ ->
     IsmUChar (pos, Char.chr (if bool then 1 else 0)), b
  | IsmUChar _, IsmBool (pos, bool) ->
     a, IsmUChar (pos, Char.chr (if bool then 1 else 0))
  | IsmBool (pos, bool), IsmInt16 _ ->
     IsmInt16 (pos, if bool then 1l else 0l), b
  | IsmInt16 _, IsmBool (pos, bool) ->
     a, IsmInt16 (pos, if bool then 1l else 0l)
  | IsmBool (pos, bool), IsmUInt16 _ ->
     IsmUInt16 (pos, if bool then 1l else 0l), b
  | IsmUInt16 _, IsmBool (pos, bool) ->
     a, IsmUInt16 (pos, if bool then 1l else 0l)
  | IsmBool (pos, bool), IsmInt32 _ ->
     IsmInt32 (pos, if bool then 1l else 0l), b
  | IsmInt32 _, IsmBool (pos, bool) ->
     a, IsmInt32 (pos, if bool then 1l else 0l)
  | IsmBool (pos, bool), IsmUInt32 _ ->
     IsmUInt32 (pos, if bool then 1l else 0l), b
  | IsmUInt32 _, IsmBool (pos, bool) ->
     a, IsmUInt32 (pos, if bool then 1l else 0l)
  | IsmBool (pos, bool), IsmInt64 _ ->
     IsmInt64 (pos, if bool then 1L else 0L), b
  | IsmInt64 _, IsmBool (pos, bool) ->
     a, IsmInt64 (pos, if bool then 1L else 0L)
  | IsmBool (pos, bool), IsmFloat32 _ ->
     IsmFloat32 (pos, if bool then 1.0 else 0.0), b
  | IsmFloat32 _, IsmBool (pos, bool) ->
     a, IsmFloat32 (pos, if bool then 1.0 else 0.0)
  | IsmBool (pos, bool), IsmFloat64 _ ->
     IsmFloat64 (pos, if bool then 1.0 else 0.0), b
  | IsmFloat64 _, IsmBool (pos, bool) ->
     a, IsmFloat64 (pos, if bool then 1.0 else 0.0)

  (* Char *)
  | IsmChar _, IsmChar _ -> a, b
  | IsmChar (pos, c), IsmInt16 _ ->
     IsmInt16 (pos, Int32.of_int (Char.code c)), b
  | IsmInt16 _, IsmChar (pos, c) ->
     a, IsmInt16 (pos, Int32.of_int (Char.code c))
  | IsmChar (pos, c), IsmUInt16 _ ->
     IsmUInt16 (pos, Int32.of_int (Char.code c)), b
  | IsmUInt16 _, IsmChar (pos, c) ->
     a, IsmUInt16 (pos, Int32.of_int (Char.code c))
  | IsmChar (pos, c), IsmInt32 _ ->
     IsmInt32 (pos, Int32.of_int (Char.code c)), b
  | IsmInt32 _, IsmChar (pos, c) ->
     a, IsmInt32 (pos, Int32.of_int (Char.code c))
  | IsmChar (pos, c), IsmUInt32 _ ->
     IsmUInt32 (pos, Int32.of_int (Char.code c)), b
  | IsmUInt32 _, IsmChar (pos, c) ->
     a, IsmUInt32 (pos, Int32.of_int (Char.code c))
  | IsmChar (pos, c), IsmInt64 _ ->
     IsmInt64 (pos, Int64.of_int (Char.code c)), b
  | IsmInt64 _, IsmChar (pos, c) ->
     a, IsmInt64 (pos, Int64.of_int (Char.code c))
  | IsmChar (pos, c), IsmUInt64 _ ->
     IsmUInt64 (pos, Int64.of_int (Char.code c)), b
  | IsmUInt64 _, IsmChar (pos, c) ->
     a, IsmUInt64 (pos, Int64.of_int (Char.code c))
  | IsmChar (pos, c), IsmFloat32 _ ->
     IsmFloat32 (pos, float_of_int (Char.code c)), b
  | IsmFloat32 _, IsmChar (pos, c) ->
     a, IsmFloat32 (pos, float_of_int (Char.code c))
  | IsmChar (pos, c), IsmFloat64 _ ->
     IsmFloat64 (pos, float_of_int (Char.code c)), b
  | IsmFloat64 _, IsmChar (pos, c) ->
     a, IsmFloat64 (pos, float_of_int (Char.code c))

  (* Int16 *)
  | IsmInt16 _, IsmInt16 _ -> a, b
  | IsmInt16 (pos, n), IsmUInt16 _ -> IsmUInt16 (pos, n), b
  | IsmUInt16 _, IsmInt16 (pos, n) -> a, IsmUInt16 (pos, n)
  | IsmInt16 (pos, n), IsmInt32 _ -> IsmInt32 (pos, n), b
  | IsmInt32 _, IsmInt16 (pos, n) -> a, IsmInt32 (pos, n)
  | IsmInt16 (pos, n), IsmUInt32 _ -> IsmUInt32 (pos, n), b
  | IsmUInt32 _, IsmInt16 (pos, n) -> a, IsmUInt32 (pos, n)
  | IsmInt16 (pos, n), IsmInt64 _ -> IsmInt64 (pos, Int64.of_int32 n), b
  | IsmInt64 _, IsmInt16 (pos, n) -> a, IsmInt64 (pos, Int64.of_int32 n)
  | IsmInt16 (pos, n), IsmUInt64 _ -> IsmUInt64 (pos, Int64.of_int32 n), b
  | IsmUInt64 _, IsmInt16 (pos, n) -> a, IsmUInt64 (pos, Int64.of_int32 n)
  | IsmInt16 (pos, n), IsmFloat32 _ -> IsmFloat32 (pos, Int32.to_float n), b
  | IsmFloat32 _, IsmInt16 (pos, n) -> a, IsmFloat32 (pos, Int32.to_float n)
  | IsmInt16 (pos, n), IsmFloat64 _ -> IsmFloat64 (pos, Int32.to_float n), b
  | IsmFloat64 _, IsmInt16 (pos, n) -> a, IsmFloat64 (pos, Int32.to_float n)

  (* UInt16 *)
  | IsmUInt16 _, IsmUInt16 _ -> a, b
  | IsmInt32 _, IsmUInt16 (pos, n) -> a, IsmInt32 (pos, n)
  | IsmUInt16 (pos, n), IsmUInt32 _ -> IsmUInt32 (pos, n), b
  | IsmUInt32 _, IsmUInt16 (pos, n) -> a, IsmUInt32 (pos, n)
  | IsmUInt16 (pos, n), IsmInt64 _ -> IsmInt64 (pos, Int64.of_int32 n), b
  | IsmInt64 _, IsmUInt16 (pos, n) -> a, IsmInt64 (pos, Int64.of_int32 n)
  | IsmUInt16 (pos, n), IsmUInt64 _ -> IsmUInt64 (pos, Int64.of_int32 n), b
  | IsmUInt64 _, IsmUInt16 (pos, n) -> a, IsmUInt64 (pos, Int64.of_int32 n)
  | IsmUInt16 (pos, n), IsmFloat32 _ -> IsmFloat32 (pos, Int32.to_float n), b
  | IsmFloat32 _, IsmUInt16 (pos, n) -> a, IsmFloat32 (pos, Int32.to_float n)
  | IsmUInt16 (pos, n), IsmFloat64 _ -> IsmFloat64 (pos, Int32.to_float n), b
  | IsmFloat64 _, IsmUInt16 (pos, n) -> a, IsmFloat64 (pos, Int32.to_float n)

  (* Int32 *)
  | IsmInt32 _, IsmInt32 _ -> a, b
  | IsmInt32 (pos, n), IsmUInt32 _ -> IsmUInt32 (pos, n), b
  | IsmUInt32 _, IsmInt32 (pos, n) -> a, IsmUInt32 (pos, n)
  | IsmInt32 (pos, n), IsmInt64 _ -> IsmInt64 (pos, Int64.of_int32 n), b
  | IsmInt64 _, IsmInt32 (pos, n) -> a, IsmInt64 (pos, Int64.of_int32 n)
  | IsmInt32 (pos, n), IsmUInt64 _ -> IsmUInt64 (pos, Int64.of_int32 n), b
  | IsmUInt64 _, IsmInt32 (pos, n) -> a, IsmUInt64 (pos, Int64.of_int32 n)
  | IsmInt32 (pos, n), IsmFloat32 _ -> IsmFloat32 (pos, Int32.to_float n), b
  | IsmFloat32 _, IsmInt32 (pos, n) -> a, IsmFloat32 (pos, Int32.to_float n)
  | IsmInt32 (pos, n), IsmFloat64 _ -> IsmFloat64 (pos, Int32.to_float n), b
  | IsmFloat64 _, IsmInt32 (pos, n) -> a, IsmFloat64 (pos, Int32.to_float n)

  (* UInt32 *)
  | IsmUInt32 _, IsmUInt32 _ -> a, b
  | IsmUInt32 (pos, n), IsmInt64 _ -> IsmInt64 (pos, Int64.of_int32 n), b
  | IsmInt64 _, IsmUInt32 (pos, n) -> a, IsmInt64 (pos, Int64.of_int32 n)
  | IsmUInt32 (pos, n), IsmUInt64 _ -> IsmUInt64 (pos, Int64.of_int32 n), b
  | IsmUInt64 _, IsmUInt32 (pos, n) -> a, IsmUInt64 (pos, Int64.of_int32 n)
  | IsmUInt32 (pos, n), IsmFloat32 _ -> IsmFloat32 (pos, Int32.to_float n), b
  | IsmFloat32 _, IsmUInt32 (pos, n) -> a, IsmFloat32 (pos, Int32.to_float n)
  | IsmUInt32 (pos, n), IsmFloat64 _ -> IsmFloat64 (pos, Int32.to_float n), b
  | IsmFloat64 _, IsmUInt32 (pos, n) -> a, IsmFloat64 (pos, Int32.to_float n)

  (* Int64 *)
  | IsmInt64 _, IsmInt64 _ -> a, b
  | IsmInt64 (pos, n), IsmUInt64 _ -> IsmUInt64 (pos, n), b
  | IsmUInt64 _, IsmInt64 (pos, n) -> a, IsmUInt64 (pos, n)
  | IsmInt64 (pos, n), IsmFloat32 _ -> IsmFloat32 (pos, Int64.to_float n), b
  | IsmFloat32 _, IsmInt64 (pos, n) -> a, IsmFloat32 (pos, Int64.to_float n)
  | IsmInt64 (pos, n), IsmFloat64 _ -> IsmFloat64 (pos, Int64.to_float n), b
  | IsmFloat64 _, IsmInt64 (pos, n) -> a, IsmFloat64 (pos, Int64.to_float n)

  (* UInt64 *)
  | IsmUInt64 _, IsmUInt64 _ -> a, b
  | IsmUInt64 (pos, n), IsmFloat32 _ -> IsmFloat32 (pos, Int64.to_float n), b
  | IsmFloat32 _, IsmUInt64 (pos, n) -> a, IsmFloat32 (pos, Int64.to_float n)
  | IsmUInt64 (pos, n), IsmFloat64 _ -> IsmFloat64 (pos, Int64.to_float n), b
  | IsmFloat64 _, IsmUInt64 (pos, n) -> a, IsmFloat64 (pos, Int64.to_float n)

  (* Float32 *)
  | IsmFloat32 _, IsmFloat32 _ -> a, b
  | IsmFloat32 (pos, n), IsmFloat64 _ -> IsmFloat64 (pos, n), b
  | IsmFloat64 _, IsmFloat32 (pos, n) -> a, IsmFloat64 (pos, n)

  (* Float64 *)
  | IsmFloat64 _, IsmFloat64 _ -> a, b

  | _ -> Ismerr.internal "non-numerical type."

let debind = function
  | IsmBinding (BBIsm ism) -> ism
  | ism -> ism

let boolean_p pos args =
  match ism_extract_single_arg "boolean?" args with
  | IsmBool _ -> IsmBool (pos, true)
  | _ -> IsmBool (pos, false)

let mathop name op_kind =
  let char i32 a b =
    let rawval =
      i32 (Int32.of_int (Char.code a)) (Int32.of_int (Char.code b))
    in
    Char.chr ((Int32.to_int rawval) mod 256)
  in
  fun pos args ->
  let binop i32 i64 float accum value =
    try
      match generalize_nums (debind accum) (debind value) with
      | IsmChar (pos, n), IsmChar (_, m) -> IsmChar (pos, char i32 n m)
      | IsmUChar (pos, n), IsmUChar (_, m) -> IsmUChar (pos, char i32 n m)
      | IsmInt16 (pos, n), IsmInt16 (_, m) -> IsmInt16 (pos, i32 n m)
      | IsmUInt16 (pos, n), IsmUInt16 (_, m) -> IsmUInt16 (pos, i32 n m)
      | IsmInt32 (pos, n), IsmInt32 (_, m) -> IsmInt32 (pos, i32 n m)
      | IsmUInt32 (pos, n), IsmUInt32 (_, m) -> IsmUInt32 (pos, i32 n m)
      | IsmInt64 (pos, n), IsmInt64 (_, m) -> IsmInt64 (pos, i64 n m)
      | IsmUInt64 (pos, n), IsmUInt64 (_, m) -> IsmUInt64 (pos, i64 n m)
      | IsmFloat32 (pos, n), IsmFloat32 (_, m) -> IsmFloat32 (pos, float n m)
      | IsmFloat64 (pos, n), IsmFloat64 (_, m) -> IsmFloat64 (pos, float n m)
      | _ -> Ismerr.internal "ISM math binop error."
    with ExNoFloatPermitted ->
      Ismerr.err_no_float_permitted pos name
  in
  match args with
  | IsmTerm _ ->
     Ismerr.err_args_mismatch pos 2 0
  | _ ->
     match op_kind with
     | Multi (i32, i64, float) ->
        let rec proc accum = function
          | IsmNode (obj, rest) ->
             let num = is_num (debind obj) in
             let accum = binop i32 i64 float accum num in
             proc accum rest
          | IsmTerm _ -> accum
          | _ -> Ismerr.internal "processing a non-list."
        in
        let first, rest = split_list args in
        proc (is_num (debind first)) rest
     | Binary (i32, i64, float) ->
        let len = sexpr_length args in
        if len <> 2 then
          Ismerr.err_binary_fcn pos name len
        else
          let p1, p2 = ism_extract_two_args name args in
          let first = is_num (debind p1) in
          let second = is_num (debind p2) in
          binop i32 i64 float first second
     | Unary _ ->
        (* FIXME! *)
        Ismerr.internal "unary not implemented, yet."

let add = mathop "+" @@ Multi (Int32.add, Int64.add, (+.))
let sub = mathop "-" @@ Multi (Int32.sub, Int64.sub, (-.))
let mul = mathop "*" @@ Multi (Int32.mul, Int64.mul, ( *. ))
let div = mathop "/" @@ Multi (Int32.div, Int64.div, (/.))
let modulo = mathop "%" @@
               Binary (Int32.rem, Int64.rem,
                       (fun _ -> raise ExNoFloatPermitted))

let string_append pos args =
  let append accum arg =
    match accum, arg with
    | IsmString (p1, s1), IsmString (_, s2) ->
       IsmString (p1, s1 ^ s2)
    | _ ->
       Ismerr.err_string_append_expected_string (pos_of_ism arg)
  in
  fold_left append (IsmString (pos, "")) args

let generic_conv name pos bool char i32 i64 float args =
  let head, tail = split_list args in
  match tail with
  | IsmTerm _ ->
     begin
       match head with
       | IsmBool (p, v) -> p, bool v
       | IsmChar (p, v)    | IsmUChar (p, v) -> p, char v
       | IsmInt16 (p, v)   | IsmUInt16 (p, v) -> p, i32 v
       | IsmInt32 (p, v)   | IsmUInt32 (p, v) -> p, i32 v
       | IsmInt64 (p, v)   | IsmUInt64 (p, v) -> p, i64 v
       | IsmFloat32 (p, v) | IsmFloat64 (p, v) -> p, float v
       | _ -> Ismerr.err_nan pos
     end
  | _ ->
     Ismerr.err_args_mismatch pos 1 (1 + (sexpr_length tail))

let ident x = x

let bool_conv name pos =
  generic_conv name pos
    ident
    (fun c -> if (Char.code c) = 0 then false else true)
    (fun n -> if Int32.equal n 0l then false else true)
    (fun n -> if Int64.equal n 0L then false else true)
    (fun n -> if n <> 0.0 then true else false)

let cchr n =
  if n > 255 then Char.chr 255
  else if n < 0 then Char.chr 0
  else Char.chr n

let char_conv name pos =
  generic_conv name pos
    (fun b -> if b then Char.chr 1 else Char.chr 0)
    ident
    (fun n -> cchr (Int32.to_int n))
    (fun n -> cchr (Int64.to_int n))
    (fun n -> cchr (int_of_float n))

let i32_conv name pos =
  generic_conv name pos
    (fun b -> if b then 1l else 0l)
    (fun c -> Int32.of_int (Char.code c))
    ident
    Int64.to_int32
    Int32.of_float

let i64_conv name pos =
  generic_conv name pos
    (fun b -> if b then 1L else 0L)
    (fun c -> Int64.of_int (Char.code c))
    Int64.of_int32
    ident
    Int64.of_float

let float_conv name pos =
  generic_conv name pos
    (fun b -> if b then 1.0 else 0.0)
    (fun c -> float_of_int (Char.code c))
    Int32.to_float
    Int64.to_float
    ident

let ident_of pos = function
  | IsmNode (IsmString (pos, str), IsmTerm _) ->
     IsmDefIdent (pos, str)
  | IsmNode (_, IsmNode _) as node ->
     Ismerr.err_args_mismatch pos 1 (sexpr_length node)
  | IsmNode (ism, _) ->
     Ismerr.err_ident_from_non_string (pos_of_ism ism)
  | ism ->
     Ismerr.err_malformed_sexpr (pos_of_ism ism)

let if_form eval bindings pos = function
  | IsmNode (cond, IsmNode (t, IsmNode (f, IsmTerm _))) ->
     begin
       match eval bindings cond with
       | IsmBool (_, false) -> eval bindings f
       | _ -> eval bindings t
     end
  | args ->
     Ismerr.err_args_mismatch pos 3 (sexpr_length args)

let let_form is_star eval bindings pos = function
  | IsmNode (list, IsmNode (expr, IsmTerm _)) ->
     let subscope = push_symtab_scope bindings in
     let bscope = if is_star then subscope
                  else bindings
     in
     let bind = function
       | IsmNode (IsmIdent id, IsmNode (e, IsmTerm _)) ->
          let binding = eval bscope e in
          add_symbol subscope id.td_text (BBIsm binding)
       | _ ->
          Ismerr.err_let_need_bindings pos is_star
     in
     let () = iter bind list in
     eval subscope expr
  | args ->
     Ismerr.err_args_mismatch pos 2 (sexpr_length args)

let quote_form _ _ pos = function
  | IsmNode (obj, IsmTerm _) -> obj
  | ism ->
     Ismerr.err_args_mismatch (pos_of_ism ism) 1 (sexpr_length ism)

let list pos args = args

let car_op pos = function
  | IsmNode (list, IsmTerm _) ->
     let head, _ = split_list list in
     head
  | ism ->
     Ismerr.err_args_mismatch (pos_of_ism ism) 1 (sexpr_length ism)

let cdr_op pos = function
  | IsmNode (list, IsmTerm _) ->
     let _, tail = split_list list in
     tail
  | ism ->
     Ismerr.err_args_mismatch (pos_of_ism ism) 1 (sexpr_length ism)

let list_ref pos = function
  | IsmNode (IsmNode _ as list, IsmNode (nobj, IsmTerm _)) ->
     let rec proc n = function
       | IsmNode (obj, rest) ->
          if (Int64.compare n 1L) > 0 then proc (Int64.sub n 1L) rest
          else obj
       | ism ->
          Ismerr.err_malformed_sexpr (pos_of_ism ism)
     in
     proc (extract_int64 nobj) list
  | _ ->
     Ismerr.err_list_ref pos

let precompute f eval bindings pos args =
  let proc accum ism = (eval bindings ism) :: accum in
  let make_list accum ism = IsmNode (ism, accum) in
  f pos @@ List.fold_left make_list (IsmTerm pos) @@ fold_left proc [] args

let map_op eval bindings pos raw_args =
  let n_args = sexpr_length raw_args in
  if n_args <> 2 then
    Ismerr.err_args_mismatch pos 2 n_args;

  let fcn, rest = split_list raw_args in
  let params, env, body =
    match eval bindings fcn with
    | IsmBinding (BBLambda (params, env, body)) -> params, env, body
    | ism -> Ismerr.err_map_needs_fcn (pos_of_ism ism)
  in
  if List.length params <> 1 then
    Ismerr.err_map_lambda_needs_one_param pos;
  let raw_list, _ = split_list rest in
  let list =
    match eval bindings raw_list with
    | IsmNode _ as list -> list
    | _ -> Ismerr.err_map_needs_list (pos_of_ism raw_list)
  in
  let param = List.hd params in
  let apply_lambda accum obj =
    let subscope = push_symtab_scope env in
    add_symbol subscope param.td_text (BBIsm obj);
    (eval subscope body) :: accum
  in
  let make_list accum obj = IsmNode (obj, accum) in
  List.fold_left make_list (IsmTerm pos) @@ fold_left apply_lambda [] list

let concat_stmts pos = function
  | IsmNode (IsmNode (IsmDefStmts stmts, rest), IsmTerm _) ->
     let proc accum = function
       | IsmDefStmts stmts -> stmts :: accum
       | ism -> Ismerr.err_concat_stmts_bad_arg (pos_of_ism ism)
     in
     IsmDefStmts (List.flatten (fold_left proc [stmts] rest))
  | _ ->
     Ismerr.err_concat_stmts_bad_arg pos

let construct_if eval bindings pos raw_args =
  let construct _ = function
    | IsmNode (list, IsmTerm _) ->
       let get_stmts ism =
         match eval bindings ism with
         | IsmDefStmts stmts -> stmts
         | _ -> Ismerr.err_construct_if_bad_arg (pos_of_ism ism)
       in
       let expr_stmt_pair = function
         | IsmNode (IsmDefExpr (tok, _) as cond,
                    IsmNode (IsmDefStmts _ as stmts,
                             IsmTerm _)) ->
            let cond =
              match eval bindings cond with
              | IsmDefExpr (_, e) -> e
              | e -> PTE_IsmExpr (tok, e)
            in
            cond, get_stmts stmts
         | ism ->
            Ismerr.err_construct_if_bad_arg (pos_of_ism ism)
       in
       let head, tail = split_list list in
       let cond, body = expr_stmt_pair head in
       let faux_tok =
         { td_text = "faux";
           td_pos = pos;
           td_noncode = []
         }
       in
       let get_clauses accum obj =
         let cond, body = expr_stmt_pair obj in
         (faux_tok, cond, faux_tok, body) :: accum
       in
       let elifs = List.rev (fold_left get_clauses [] tail) in
       IsmDefStmts
         [ PTS_IfStmt (faux_tok, cond, faux_tok, body,
                       elifs, None, faux_tok)
         ]
    | _ ->
       Ismerr.err_construct_if_bad_arg pos
  in
  precompute construct eval bindings pos raw_args

let ism_builtins =
  [ (*-- Types --*)
    ("boolean?", precompute boolean_p);

    (*-- Math Operations --*)
    ("+", precompute add); ("-", precompute sub);
    ("*", precompute mul); ("/", precompute div);
    ("%", precompute modulo);

    (*-- String Operations --*)
    ("string-append", precompute string_append);

    (*-- Conversions --*)
    ("bool",
     precompute
       (fun pos args ->
         let p, v = bool_conv "bool" pos args in IsmBool (p, v)));
    ("char",
     precompute
       (fun pos args ->
         let p, v = char_conv "char" pos args in IsmChar (p, v)));
    ("uchar",
     precompute
       (fun pos args ->
         let p, v = char_conv "uchar" pos args in IsmUChar (p, v)));
    ("i16",
     precompute
       (fun pos args ->
         let p, v = i32_conv "i16" pos args in IsmInt16 (p, v)));
    ("u16",
     precompute
       (fun pos args ->
         let p, v = i32_conv "u16" pos args in IsmUInt16 (p, v)));
    ("i32",
     precompute
       (fun pos args ->
         let p, v = i32_conv "i32" pos args in IsmInt32 (p, v)));
    ("u32",
     precompute
       (fun pos args ->
         let p, v = i32_conv "u32" pos args in IsmUInt32 (p, v)));
    ("i64",
     precompute
       (fun pos args ->
         let p, v = i64_conv "i64" pos args in IsmInt64 (p, v)));
    ("u64",
     precompute
       (fun pos args ->
         let p, v = i64_conv "u64" pos args in IsmUInt64 (p, v)));
    ("float32",
     precompute
       (fun pos args ->
         let p, v = float_conv "float32" pos args in IsmFloat32 (p, v)));
    ("float64",
     precompute
       (fun pos args ->
         let p, v = float_conv "float64" pos args in IsmFloat64 (p, v)));
    ("ident", precompute ident_of);

    (*-- Special Forms --*)
    ("if", if_form);
    ("let", let_form false);
    ("let*", let_form true);
    ("quote", quote_form);

    (*-- List Operations --*)
    ("list", precompute list);
    ("car", precompute car_op);
    ("cdr", precompute cdr_op);
    ("list-ref", precompute list_ref);
    ("map", map_op);

    (*-- Statement Operations --*)
    ("concat-stmts", precompute concat_stmts);
    ("construct-if", construct_if);
  ]

(** Return the default set of bindings. *)
let bindings_create () =
  let bindings = make_symtab () in
  List.iter (fun (k, v) -> add_symbol bindings k (BBNative v)) ism_builtins;
  bindings

(** Interpret a ISM expression and return the result. *)
let rec eval_ism bindings = function
  | IsmNode (obj, args) ->
     let pos = pos_of_ism obj in
     begin
       match eval_ism bindings obj with
       | IsmBinding (BBNative native_f) ->
          native_f eval_ism bindings pos args
       | IsmBinding (BBLambda (params, env, body)) ->
          let n_params = List.length params
          and n_profile = sexpr_length args in
          if n_params <> n_profile then
            Ismerr.err_args_mismatch pos n_params n_profile
          else
            let subscope = push_symtab_scope env in
            let rec bind params = function
              | IsmTerm _ -> ()
              | IsmNode (arg, rest) ->
                 let binding =
                   match eval_ism bindings arg with
                   | IsmBinding (BBIsm v) -> v
                   | v -> v
                 in
                 let param = List.hd params in
                 add_symbol subscope param.td_text (BBIsm binding);
                 bind (List.tl params) rest
              | ism ->
                 Ismerr.err_malformed_sexpr (pos_of_ism ism)
            in
            bind params args;
            eval_ism subscope body
       | _ ->
          Ismerr.err_called_non_fcn pos
     end
  | IsmTerm pos ->
     Ismerr.err_eval_empty_sexpr pos
  | (IsmString _) as v -> v
  | (IsmBool _) as v -> v
  | (IsmChar _) as v -> v
  | (IsmUChar _) as v -> v
  | (IsmInt16 _) as v -> v
  | (IsmUInt16 _) as v -> v
  | (IsmInt32 _) as v -> v
  | (IsmUInt32 _) as v -> v
  | (IsmInt64 _) as v -> v
  | (IsmUInt64 _) as v -> v
  | (IsmFloat32 _) as v -> v
  | (IsmFloat64 _) as v -> v
  | IsmIdent tok ->
     begin
       match lookup_symbol bindings tok.td_text with
       | Some binding ->
          begin
            match binding with
            | BBIsm bbism -> bbism
            | _ -> IsmBinding binding
          end
       | None ->
          Ismerr.err_unknown_symbol tok.td_pos tok.td_text
     end
  | IsmDefStmts stmts ->
     IsmDefStmts (List.map (resolve_stmt bindings) stmts)
  | IsmDefExpr (tok, expr) ->
     IsmDefExpr (tok, resolve_expr bindings expr)
  | IsmDefIdent _ as v -> v
  | IsmBinding binding ->
     Ismerr.internal "Unexpected binding."

and resolve_stmt bindings stmt =
  let map_apply = List.map (resolve_stmt bindings) in
  let process_for_init = function
    | PTForInit_Var (v, id, tp, eq, expr) ->
       PTForInit_Var (v, id, tp, eq, resolve_expr bindings expr)
    | PTForInit_Expr expr ->
       PTForInit_Expr (resolve_expr bindings expr)
  in
  let resolve_decl decl =
    let ex, def, ident, tp = decl in
    ex, def, IdentTok (tok_of_ident bindings ident), tp
  in
  match stmt with
  | PTS_ISM_Stmts stmts -> PTS_ISM_Stmts (map_apply stmts)
  | PTS_ISM_DelayedStmts (at, ism) ->
     begin
       match eval_ism bindings ism with
       | IsmDefStmts stmts ->
          PTS_ISM_Stmts (map_apply stmts)
       | IsmDefExpr _ ->
          Ismerr.internal "Should not see IsmDefExpr here."
       | _ ->
          Ismerr.err_emit_stmts_did_not_emit_stmts at.td_pos
     end
  | PTS_ISM_Expr _ ->
     Ismerr.internal "Shouldn't be seeing a PTS_ISM_Expr here."
  | PTS_Import _ -> stmt
  | PTS_Begin (b, stmts, e) -> PTS_Begin (b, map_apply stmts, e)
  | PTS_FcnDefExpr (decl, eq, expr, semi) ->
     PTS_FcnDefExpr (resolve_decl decl, eq, resolve_expr bindings expr, semi)
  | PTS_FcnDefBlock (decl, stmt) ->
     PTS_FcnDefBlock (resolve_decl decl, resolve_stmt bindings stmt)
  | PTS_FcnDecl _ -> stmt
  | PTS_Expr (expr, semi) -> PTS_Expr (resolve_expr bindings expr, semi)
  | PTS_Var _ -> stmt
  | PTS_VarInit (v, ids, tp, eq, exprs, semi) ->
     PTS_VarInit (v, ids, tp, eq, List.map (resolve_expr bindings) exprs, semi)
  | PTS_VarInlineStruct (v, oc, vars, cc, eq, expr, semi) ->
     PTS_VarInlineStruct
       (v, oc, vars, cc, eq, resolve_expr bindings expr, semi)
  | PTS_VarInlineStructInferred (v, oc, vars, cc, eq, expr, semi) ->
     PTS_VarInlineStructInferred
       (v, oc, vars, cc, eq, resolve_expr bindings expr, semi)
  | PTS_DeleteExpr (del, expr, semi) ->
     PTS_DeleteExpr (del, resolve_expr bindings expr, semi)
  | PTS_RetireExpr (retire, expr, semi) ->
     PTS_RetireExpr (retire, resolve_expr bindings expr, semi)
  | PTS_Transaction (atomic, b, body, cases, e) ->
     let process_case (c, e, colon, stmts) =
       c, resolve_expr bindings e, colon, map_apply stmts
     in
     PTS_Transaction
       (atomic, b, map_apply body, List.map process_case cases, e)
  | PTS_IfStmt (iftok, cond, thentok, body, elifs, default, fi) ->
     let process_elif (elif, cond, thentok, body) =
       elif, resolve_expr bindings cond, thentok, map_apply body
     in
     let process_else (elsetok, body) = elsetok, map_apply body in
     PTS_IfStmt (iftok, resolve_expr bindings cond, thentok,
                 map_apply body,
                 List.map process_elif elifs,
                 option_map process_else default,
                 fi)
  | PTS_ForLoop (fortok, init, semi1, cond, semi2, iter, dotok, body, od) ->
     PTS_ForLoop (fortok, option_map process_for_init init, semi1,
                  resolve_expr bindings cond, semi2,
                  option_map (resolve_expr bindings) iter, dotok,
                  map_apply body, od)
  | PTS_ParforLoop (parfor, init, semi1, cond, semi2, iter, dotok, body, od) ->
     PTS_ParforLoop (parfor, option_map process_for_init init, semi1,
                     resolve_expr bindings cond, semi2,
                     option_map (resolve_expr bindings) iter, dotok,
                     map_apply body, od)
  | PTS_WhileLoop (wtok, cond, dotok, body, od) ->
     PTS_WhileLoop (wtok, resolve_expr bindings cond, dotok,
                    map_apply body, od)
  | PTS_DoWhileLoop (dotok, body, od, wtok, cond, semi) ->
     PTS_DoWhileLoop (dotok, map_apply body,
                      od, wtok, resolve_expr bindings cond, semi)
  | PTS_SwitchStmt (switch, pattern, withtok, cases, esac) ->
     let process_case = function
       | PTMatchCase (case, expr, colon, stmts) ->
          PTMatchCase (case, resolve_expr bindings expr, colon,
                       map_apply stmts)
       | PTFallCase (case, expr, colon, stmts) ->
          PTFallCase (case, resolve_expr bindings expr, colon,
                      map_apply stmts)
     in
     PTS_SwitchStmt (switch, resolve_expr bindings pattern,
                     withtok, List.map process_case cases, esac)
  | PTS_ReturnExpr (ret, expr, semi) ->
     PTS_ReturnExpr (ret, resolve_expr bindings expr, semi)
  | PTS_Type _ -> stmt
  | PTS_Return _ -> stmt
  | PTS_Goto _ -> stmt
  | PTS_Break _ -> stmt
  | PTS_Label _ -> stmt
  | PTS_Continue _ -> stmt
  | PTS_Sync _ -> stmt

and resolve_expr bindings expr =
  let rec resolve e =
    match e with
    | PTE_IsmExpr (tok, ism) -> PTE_IsmExpr (tok, eval_ism bindings ism)
    | PTE_New (newtok, tp, field_inits) ->
       let process_fi fi =
         { ptfi_fname = fi.ptfi_fname;
           ptfi_colon = fi.ptfi_colon;
           ptfi_expr = resolve fi.ptfi_expr
         }
       in
       let process_filist (oc, filist, cc) =
         oc, List.map process_fi filist, oc
       in
       PTE_New (newtok, tp, option_map process_filist field_inits)
    | PTE_Nil _ -> e
    | PTE_Type _ -> e
    | PTE_I64 _ -> e | PTE_U64 _ -> e
    | PTE_I32 _ -> e | PTE_U32 _ -> e
    | PTE_I16 _ -> e | PTE_U16 _ -> e
    | PTE_I8 _ -> e  | PTE_U8 _ -> e
    | PTE_Bool _ -> e
    | PTE_F64 _ -> e | PTE_F32 _ -> e
    | PTE_String _ -> e
    | PTE_Wildcard _ -> e
    | PTE_FcnCall fcn ->
       PTE_FcnCall
         { ptfc_spawn = fcn.ptfc_spawn;
           ptfc_name = IdentTok (tok_of_ident bindings fcn.ptfc_name);
           ptfc_lparen = fcn.ptfc_lparen;
           ptfc_args = List.map resolve fcn.ptfc_args;
           ptfc_rparen = fcn.ptfc_rparen
         }
    | PTE_Cast (cast, tp, lp, expr, rp) ->
       PTE_Cast (cast, tp, lp, resolve expr, rp)
    | PTE_Var var ->
       PTE_Var (IdentTok (tok_of_ident bindings var))
    | PTE_StaticStruct (name, oc, exprs, cc) ->
       PTE_StaticStruct (name, oc, List.map resolve exprs, cc)
    | PTE_StaticArray (os, exprs, cs) ->
       PTE_StaticArray (os, List.map resolve exprs, cs)
    | PTE_Index (base, os, idx, cs) ->
       PTE_Index (resolve base, os, resolve idx, cs)
    | PTE_SelectField (obj, dot, field) ->
       PTE_SelectField (resolve obj, dot, field)
    | PTE_PostUni (expr, oper) -> PTE_PostUni (resolve expr, oper)
    | PTE_PreUni (oper, expr) -> PTE_PreUni (oper, resolve expr)
    | PTE_Bin (left, atomic, oper, right) ->
       PTE_Bin (resolve left, atomic, oper, resolve right)
    | PTE_TernaryCond (cond, qmark, texpr, colon, fexpr) ->
       PTE_TernaryCond
         (resolve cond, qmark, resolve texpr, colon, resolve fexpr)
  in
  resolve expr

(** Return a token from an ident.  This will generate an internal error
    if the ident is still an unevaluated ISM. *)
and tok_of_ident bindings = function
  | IdentTok tok -> tok
  | IdentIsm (pos, ism) ->
     match eval_ism bindings ism with
     | IsmIdent tok -> tok
     | IsmString (pos, str) ->
        { td_pos = pos;
          td_text = str;
          td_noncode = [];
        }
     | _ -> Ismerr.err_non_ident_tok pos
