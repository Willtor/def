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
