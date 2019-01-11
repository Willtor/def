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

let is_num ism =
  match ism with
  | IsmChar _ | IsmUChar _
  | IsmInt16 _ | IsmUInt16 _
  | IsmInt32 _ | IsmUInt32 _
  | IsmInt64 _ | IsmUInt64 _
  | IsmFloat32 _ | IsmFloat64 _ -> ism
  | IsmBinding _ -> Error.fatal_error "is_num found a binding."
  | _ -> Error.fatal_error "ISM value isn't a number."

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

  | _ -> Error.fatal_error "non-numerical type."

let debind = function
  | IsmBinding (BBIsm ism) -> ism
  | ism -> ism

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
    | _ -> Error.fatal_error "Internal error in a ISM mathop."
  in
  if args = [] then
    Error.fatal_error ("need suitable error for noargs ISM " ^ name)
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
  | [IsmBool (p, v)] -> p, bool v
  | [IsmChar (p, v)]    | [IsmUChar (p, v)] -> p, char v
  | [IsmInt16 (p, v)]   | [IsmUInt16 (p, v)] -> p, i32 v
  | [IsmInt32 (p, v)]   | [IsmUInt32 (p, v)] -> p, i32 v
  | [IsmInt64 (p, v)]   | [IsmUInt64 (p, v)] -> p, i64 v
  | [IsmFloat32 (p, v)] | [IsmFloat64 (p, v)] -> p, float v
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

let ism_builtins =
  [ (*-- Operations --*)
    ("+", add); ("-", sub);
    ("*", mul); ("/", div);
    ("%", modulo);

    (*-- Conversions --*)
    ("bool",
     (fun pos args -> let p, v = bool_conv pos args in IsmBool (p, v)));
    ("char",
     (fun pos args -> let p, v = char_conv pos args in IsmChar (p, v)));
    ("uchar",
     (fun pos args -> let p, v = char_conv pos args in IsmUChar (p, v)));
    ("i16",
     (fun pos args -> let p, v = i32_conv pos args in IsmInt16 (p, v)));
    ("u16",
     (fun pos args -> let p, v = i32_conv pos args in IsmUInt16 (p, v)));
    ("i32",
     (fun pos args -> let p, v = i32_conv pos args in IsmInt32 (p, v)));
    ("u32",
     (fun pos args -> let p, v = i32_conv pos args in IsmUInt32 (p, v)));
    ("i64",
     (fun pos args -> let p, v = i64_conv pos args in IsmInt64 (p, v)));
    ("u64",
     (fun pos args -> let p, v = i64_conv pos args in IsmUInt64 (p, v)));
    ("float32",
     (fun pos args -> let p, v = float_conv pos args in IsmFloat32 (p, v)));
    ("float64",
     (fun pos args -> let p, v = float_conv pos args in IsmFloat64 (p, v)));
  ]

(** Return the default set of bindings. *)
let bindings_create () =
  let bindings = make_symtab () in
  List.iter (fun (k, v) -> add_symbol bindings k (BBNative v)) ism_builtins;
  bindings

(** Interpret a ISM expression and return the result. *)
let rec eval_ism bindings = function
  | IsmSexpr (_, []) ->
     Error.fatal_error "empty s-expression."
  | IsmSexpr (pos, sexpr) ->
     begin
       match eval_ism bindings (List.hd sexpr) with
       | IsmBinding (BBNative native_f) ->
          native_f pos (List.tl (List.map (eval_ism bindings) sexpr))
       | IsmBinding (BBLambda (params, env, body)) ->
          if List.length params <> List.length (List.tl sexpr) then
            Error.fatal_error "unequal # of parameters in ISM function"
          else
            let subscope = push_symtab_scope env in
            let bind a b =
              let binding =
                match eval_ism bindings b with
                | IsmBinding (BBIsm v) -> v
                | v -> v
              in
              add_symbol subscope a.td_text (BBIsm binding)
            in
            let () = List.iter2 bind params (List.tl sexpr) in
            eval_ism subscope body
       | _ ->
          Error.fatal_error
            "Need suitable error: tried to call a non-function"
     end
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
          let () = symtab_iter (fun str _ -> prerr_endline str) bindings in
          Error.fatal_error
            ("FIXME: suitable error for unknown sym: " ^ tok.td_text)
     end
  | IsmDefStmts stmts ->
     IsmDefStmts (List.map (resolve_stmt bindings) stmts)
  | IsmBinding binding ->
     match binding with
     | BBIsm _ -> Error.fatal_error "bbism"
     | BBNative _ -> Error.fatal_error "bbnative"
     | BBLambda _ -> Error.fatal_error "bblambda"

and resolve_stmt bindings stmt =
  let map_apply = List.map (resolve_stmt bindings) in
  let process_for_init = function
    | PTForInit_Var (v, id, tp, eq, expr) ->
       PTForInit_Var (v, id, tp, eq, resolve_expr bindings expr)
    | PTForInit_Expr expr ->
       PTForInit_Expr (resolve_expr bindings expr)
  in
  match stmt with
  | PTS_ISM_Stmts stmts -> PTS_ISM_Stmts (map_apply stmts)
  | PTS_Import _ -> stmt
  | PTS_Begin (b, stmts, e) -> PTS_Begin (b, map_apply stmts, e)
  | PTS_FcnDefExpr (decl, eq, expr, semi) ->
     PTS_FcnDefExpr (decl, eq, resolve_expr bindings expr, semi)
  | PTS_FcnDefBlock (decl, stmt) ->
     PTS_FcnDefBlock (decl, resolve_stmt bindings stmt)
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
           ptfc_name = fcn.ptfc_name;
           ptfc_lparen = fcn.ptfc_lparen;
           ptfc_args = List.map resolve fcn.ptfc_args;
           ptfc_rparen = fcn.ptfc_rparen
         }
    | PTE_Cast (cast, tp, lp, expr, rp) ->
       PTE_Cast (cast, tp, lp, resolve expr, rp)
    | PTE_Var _ -> e
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