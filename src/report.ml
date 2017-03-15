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

open Util

(** Report an internal error. *)
let err_internal file line msg =
  let err = "Internal error: file \"" ^ file ^ "\" line "
    ^ (string_of_int line) ^ ".\n"
    ^ msg
  in fatal_error err

(** Input paramter. *)
let err_param msg =
  let err = "Compilation halted based on command line input:\n"
    ^ "  " ^ msg ^ "\n"
    ^ "Use --help for options."
  in fatal_error err

(** User forgot to specify an input file. *)
let err_no_input_file () =
  let err = "No input file specified.  Use --help for options."
  in fatal_error err

(** Input file has unknown type. *)
let err_unknown_infile_type filename ext =
  let err = "Unknown type of file \"" ^ filename ^ "\".\n"
    ^ "The filename extension, \"" ^ ext ^ "\" is not supported."
  in fatal_error err

(** Couldn't open file. *)
let err_unable_to_open_file filename =
  let err = "Unable to open file \"" ^ filename ^ "\""
  in fatal_error err

(** Lexing error. *)
let err_lexing pos character =
  let err = "At " ^ (format_position pos) ^ ":\n"
    ^ "  Unexpected character: " ^ character ^ "\n"
    ^ (show_source pos)
  in fatal_error err

(** Syntax error. *)
let err_syntax pos =
  let err = "At " ^ (format_position pos) ^ ":\n"
    ^ "  Syntax error:\n"
    ^ (show_source pos)
  in fatal_error err

(** Escaped character was unknown. *)
let err_bad_escaped_char pos c =
  let err = "At " ^ (format_position pos) ^ ":\n"
    ^ "  Escaped (backslash'd) the char, " ^ (Char.escaped c)
    ^ ", which is not recognized.\n"
    ^ (show_source pos)
  in fatal_error err

(** The ... in a parameter list did not appear last. *)
let err_vararg_not_last pos =
  let err = "At " ^ (format_position pos) ^ ":\n"
    ^ "  The ellipsis denoting a variadic function must appear at the end\n"
    ^ "  of the parameter list.\n"
    ^ (show_source pos)
  in fatal_error err

(** Initializing variables: There was a mismatch between the count of
    variables being declared and initializers. *)
let err_var_decl_list_length_mismatch pos vars inits =
  let err = "At " ^ (format_position pos) ^ ":\n"
    ^ "  Mismatch between count of variables (" ^ (string_of_int vars)
    ^ ") and initializers (" ^ (string_of_int inits) ^ ").\n"
    ^ (show_source pos)
  in fatal_error err

(** Function has no return statement. *)
let err_no_return pos fcn =
  let err = "At " ^ (format_position pos) ^ ":\n"
    ^ "  Function " ^ fcn ^ " needs a return statement.\n"
    ^ (show_source pos)
  in fatal_error err

(** Local function was declared "export" which is not allowed. *)
let err_local_fcn_with_nonlocal_vis pos fcn =
  let err = "At " ^ (format_position pos) ^ ":\n"
    ^ "  Function " ^ fcn ^ " has non-global scope, and must have local "
    ^ "visibility.\n"
    ^ (show_source pos)
  in fatal_error err

(** Used an undefined variable. *)
let err_undefined_var pos name =
  let err = "At " ^ (format_position pos) ^ ":\n"
    ^ "  Variable " ^ name ^ " is undefined.\n"
    ^ (show_source pos)
  in fatal_error err

(** Error: Tried to call an unknown function. *)
let err_unknown_fcn_call pos name =
  let err = "At " ^ (format_position pos) ^ ":\n"
    ^ "  Unknown function \"" ^ name ^ "\".\n"
    ^ (show_source pos)
  in fatal_error err

(** Tried to call a function with the wrong number of arguments. *)
let err_wrong_number_of_args use_pos decl_pos name n_params n_args =
  let err = "At " ^ (format_position use_pos) ^ ":\n"
    ^ "  Function call of " ^ name ^ " with " ^ (string_of_int n_args)
    ^ " arguments.\n"
    ^ (show_source use_pos) ^ "\n"
    ^ "  But " ^ name ^ " was declared with " ^ (string_of_int n_params)
    ^ " parameters at: " ^ (format_position decl_pos) ^ "\n"
    ^ (show_source decl_pos)
  in fatal_error err

(** Called an atomic (builtin) function with the wrong number of args. *)
let err_wrong_number_of_atomic_args pos name n_args =
  let err = "At " ^ (format_position pos) ^ ":\n"
    ^ "  Atomic function " ^ name ^ " called with " ^ (string_of_int n_args)
    ^ " arguments.\n"
    ^ (show_source pos) ^ "\n"
  in fatal_error err

(** Tried to call a variable that was not a function. *)
let err_called_non_fcn use_pos decl_pos name =
  let err = "At " ^ (format_position use_pos) ^ ":\n"
    ^ "  Called " ^ name ^ ", which was not declared as a function.\n"
    ^ (show_source use_pos) ^ "\n"
    ^ "  Declared at " ^ (format_position decl_pos) ^ "\n"
    ^ (show_source decl_pos)
  in fatal_error err

(** An operator was used on objects of incompatible types. *)
let err_not_same_type pos op ltype rtype =
  let err = "At " ^ (format_position pos) ^ ":\n"
    ^ "  Operator " ^ op ^ " applied to different types:\n"
    ^ "  " ^ ltype ^ " and " ^ rtype ^ "\n"
    ^ (show_source pos)
  in fatal_error err

(** Declared a new variable with the same name as another one in the same
    scope. *)
let err_redeclared_variable pos orig_pos var =
  let err = "At " ^ (format_position pos) ^ ":\n"
    ^ "  Redeclared variable \"" ^ var ^ "\".\n"
    ^ (show_source pos) ^ "\n"
    ^ "  Originally declared at " ^ (format_position orig_pos) ^ "\n"
    ^ (show_source pos)
  in fatal_error err

(** User specified an unknown or undeclared type. *)
let err_unknown_typename pos name =
  let err = "At " ^ (format_position pos) ^ ":\n"
    ^ "  Unknown type name: " ^ name ^ ".\n"
    ^ (show_source pos)
  in fatal_error err

(** Returned void in a non-void function.
    FIXME: This msg could be better. *)
let err_returned_void pos =
  let err = "At " ^ (format_position pos) ^ ":\n"
    ^ "  Returned void in a non-void function.\n"
    ^ (show_source pos)
  in fatal_error err

(** Tried to dereference a void pointer. *)
let err_deref_void_ptr bpos ipos =
  let err = "At " ^ (format_position bpos) ^ ":\n"
    ^ "  Can't dereference a pointer to void.  Need to cast, first.\n"
    ^ (show_source bpos) ^ "\n"
    ^ "  Offending index at " ^ (format_position ipos) ^ ":\n"
    ^ (show_source ipos)
  in fatal_error err

(** Tried to index a pointer with a non-integer index.
    FIXME: Error message should include type of the index. *)
let err_non_integer_index pos =
  let err = "At " ^ (format_position pos) ^ ":\n"
    ^ "  The index is not an integer.\n"
    ^ (show_source pos)
  in fatal_error err

(** Indexed a non-pointer.
    FIXME: Error message should include type of base. *)
let err_index_non_ptr pos =
  let err = "At " ^ (format_position pos) ^ ":\n"
    ^ "  Can't index into something that isn't a pointer.\n"
    ^ (show_source pos)
  in fatal_error err

(** Type contains itself -- not as a pointer, but as the raw type. *)
let err_recursive_type_defn pos name =
  let err = "At " ^ (format_position pos) ^ ":\n"
    ^ "  Recursive defintion of type " ^ name ^ ".\n"
    ^ (show_source pos)
  in fatal_error err

let err_struct_not_enough_fields pos n =
  let err = "At " ^ (format_position pos) ^ ":\n"
    ^ "  Field number " ^ (string_of_int n) ^ " is too high.\n"
    ^ "  Note: Field numbers are zero-indexed.\n"
    ^ (show_source pos)
  in fatal_error err

(** Tried to access a struct member that doesn't exist in the struct. *)
let err_struct_no_such_member pos mname =
  let err = "At " ^ (format_position pos) ^ ":\n"
    ^ "  Struct has no such member: " ^ mname ^ ".\n"
    ^ (show_source pos)
  in fatal_error err

(** Tried to access a member of an object that was not a struct.
    FIXME: Should name the type. *)
let err_non_struct_member_access pos =
  let err = "At " ^ (format_position pos) ^ ":\n"
    ^ "  Object is not a struct and has no members to access.\n"
    ^ (show_source pos)
  in fatal_error err

(** Expression's type doesn't match the expected type.
    FIXME: Should identify the two types. *)
let err_type_mismatch pos =
  let err = "At " ^ (format_position pos) ^ ":\n"
    ^ "  Type mismatch.\n"
    ^ (show_source pos)
  in fatal_error err
