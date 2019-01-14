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

(** Undefined variable. *)
let err_unknown_symbol pos sym =
  err_pos ("Unknown symbol: " ^ sym) pos

(** Tried to perform a numeric operation on a non-numeric value. *)
let err_nan pos =
  err_pos "Value is not a number." pos

(** Called a non-function *)
let err_called_non_fcn pos =
  err_pos "Tried to call a non-function." pos

(** fcn was called with no arguments. *)
let err_no_args_present pos fcn =
  err_pos (fcn ^ " requires arguments.") pos

(** Too many arguments were supplied to the function. *)
let err_too_many_args pos fcn =
  err_pos (fcn ^ ": too many arguments specified.") pos

(** Expected one argument, got <> 1. *)
let err_expected_one_arg pos fcn =
  err_pos (fcn ^ " expected one argument.") pos

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
