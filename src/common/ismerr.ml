(* Copyright (C) 2019  DEFC Authors

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

open Error

(** Report an internal error. *)
let internal str =
  let bt = Printexc.raw_backtrace_to_string (Printexc.get_raw_backtrace ()) in
  fatal_error ("ISM internal error: " ^ str ^ "\n\nBacktrace:\n" ^ bt)

(** Syntax error. *)
let err_syntax pos str =
  err_pos ("Unexpected token in ISM: " ^ str) pos

(** Undefined variable. *)
let err_unknown_symbol pos sym =
  err_pos ("Unknown symbol: " ^ sym) pos

(** Tried to perform a numeric operation on a non-numeric value. *)
let err_nan pos =
  err_pos "Value is not a number." pos

(** Called a non-function *)
let err_called_non_fcn pos =
  err_pos "Tried to call a non-function." pos

(** Wrong number of arguments. *)
let err_args_mismatch pos expected got =
  err_pos
    ("Expected # args = " ^ (string_of_int expected)
     ^ "; got " ^ (string_of_int got))
    pos

(** Binary function was called with num_args (!= 2) *)
let err_binary_fcn pos fcn num_args =
  err_pos
    (fcn ^ " is a binary function.  Number of arguments = "
     ^ (string_of_int num_args))
    pos

(** The function doesn't allow floating point arguments (modulo, for ex.). *)
let err_no_float_permitted pos fcn =
  err_pos (fcn ^ " does not apply to floating point values.") pos

(** A list operation failed. *)
let err_list_op_failed pos fcn =
  err_pos ("Operation " ^ fcn ^ " failed on the list.") pos

(** Tried to perform a list operation on a non-list. *)
let err_list_op_on_non_list pos fcn =
  err_pos ("Can't perform " ^ fcn ^ " on a non-list") pos

(** E.g., @[] *)
let err_eval_empty_sexpr pos =
  err_pos "Tried to evaluate an empty s-expression." pos

(** Tried to convert a non-string type to an identifier. *)
let err_ident_from_non_string pos =
  err_pos "Can't convert a non-string to an identifier." pos

(** ISM expression didn't resolve to an ident where it should have. *)
let err_non_ident_tok pos =
  err_pos "Couldn't resolve identifier token." pos

(** string-append was supplied a non-string argument. *)
let err_string_append_expected_string pos =
  err_pos "strcat expected a string." pos

(** map's second argument was not a list. *)
let err_map_needs_list pos =
  err_pos "map expected a list for its second argument." pos

(** map's first argument was not a function. *)
let err_map_needs_fcn pos =
  err_pos "map expected a function for its first argument." pos

(** map was given a lambda with #args <> 1. *)
let err_map_lambda_needs_one_param pos =
  err_pos "map requires its function to take one parameter." pos

(** concat-stmts was given something other than a list of DEF statements. *)
let err_concat_stmts_bad_arg pos =
  err_pos "concat-stmts requires a list of DEF statements as an argument." pos

(** The emit-stmts function generated something other than statements. *)
let err_emit_stmts_did_not_emit_stmts pos =
  err_pos "emit-stmts did not emit statements." pos

(** let function got something other than a list of bindings. *)
let err_let_need_bindings pos is_star =
  let name = if is_star then "let*" else "let" in
  err_pos (name ^ ": first argument should be a list of bindings.") pos
