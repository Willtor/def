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

(** Couldn't find the file to open. *)
let err_unable_to_locate_imported_file pos filename =
  let err = "At " ^ (format_position pos) ^ ":\n"
    ^ "  Unable to locate file to import: " ^ filename ^ "\n"
    ^ (show_source pos)
  in fatal_error err

(** Tried to generate a header file from a non-def source file. *)
let err_cant_generate_header_from filename =
  let err = "Unable to generate a header from non-def source file \""
            ^ filename ^ "\""
  in fatal_error err

(** Tried to generate an LLVM .ll file from .s assembly. *)
let err_cant_convert_s_to_ll filename =
  let err = "Unable to convert assembly file \"" ^ filename
            ^ "\" to a .ll format."
  in fatal_error err

(** Tried to compile a file of some unsupported format into an ASM. *)
let err_cant_generate_asm_from filename =
  let err = "Unable to compile \"" ^ filename
            ^ "\" down to a .o object file."
  in fatal_error err

(** Tried to compile a file of some unsupported format into an object. *)
let err_cant_generate_obj_from filename =
  let err = "Unable to compile \"" ^ filename
            ^ "\" down to a .o object file."
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

(** User specified a variable with no type and no intializer. *)
let err_no_init_on_inferred_type pos =
  let err = "At " ^ (format_position pos) ^ ":\n"
    ^ "  Cannot infer variable type without an initializer.\n"
    ^ (show_source pos)
  in fatal_error err

let err_multiple_volatile_keywords pos =
  let err = "At " ^ (format_position pos) ^ ":\n"
    ^ "  Keyword 'volatile' cannot be specified multiple times.  "
    ^ "This is probably a typo.\n"
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

(** Redefined a variable. *)
let err_redefined_var pos orig_pos name =
  let err = "At " ^ (format_position pos) ^ ":\n"
    ^ "  Variable " ^ name ^ " was redefined.\n"
    ^ (show_source pos) ^ "\n"
    ^ "  Original definition at " ^ (format_position orig_pos) ^ ":\n"
    ^ (show_source orig_pos)
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

(** Tried to perform an atomic operation on a non-integer, non-pointer type. *)
let err_atomic_non_integer pos typename =
  let err = "At " ^ (format_position pos) ^ ":\n"
    ^ "  Cannot perform an atomic operation on type " ^ typename ^ ".\n"
    ^ (show_source pos) ^ "\n"
  in fatal_error err

(** Called an atomic function, and the dest was not a pointer. *)
let err_atomic_dest_not_ptr pos name =
  let err = "At " ^ (format_position pos) ^ ":\n"
    ^ "  Atomic function " ^ name ^ " takes a pointer as its first argument.\n"
    ^ (show_source pos) ^ "\n"
  in fatal_error err

(** parfor was written without an iteration expression.  It requires this since
    this is the only thing that happens outside the parallel region. *)
let err_parfor_needs_iter pos =
  let err = "At " ^ (format_position pos) ^ ":\n"
    ^ "  A parfor requires an iteration expression.\n"
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

(** Arrays can only have dimensions expressed in integers. *)
let err_float_array_dim pos =
  let err = "At " ^ (format_position pos) ^ ":\n"
    ^ "  Array has dimension of type float.\n"
    ^ (show_source pos)
  in fatal_error err

(** Couldn't resolve the array dimension. *)
let err_cant_resolve_array_dim pos =
  let err = "At " ^ (format_position pos) ^ ":\n"
    ^ "  Unable to statically resolve the dimension of the array.\n"
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

(** Tried to perform a modulo (%) on a non-integer type. *)
let err_modulo_on_non_integer pos ltype rtype =
  let err = "At " ^ (format_position pos) ^ ":\n"
    ^ "  Modulo operator (%) can only be performed on integer types,\n"
    ^ "  but the type here is " ^ ltype ^ " % " ^ rtype ^ ".\n"
    ^ (show_source pos)
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

(** Type expression in unexpected (disallowed) location. *)
let err_unexpected_type_expr pos =
  let err = "At " ^ (format_position pos) ^ ":\n"
    ^ "  Unexpected type expression.\n"
    ^ (show_source pos)
  in fatal_error err

(** Builtin function doesn't allow the given format. *)
let err_bad_args_for_builtin pos builtin_fcn =
  let err = "At " ^ (format_position pos) ^ ":\n"
    ^ "  Format not allowed for builtin function: " ^ builtin_fcn ^ "\n"
    ^ (show_source pos)
  in fatal_error err

(** Tried to spawn internally in an expression or in a condition or some
    such disallowed place. *)
let err_bad_spawn_loc pos =
  let err = "At " ^ (format_position pos) ^ ":\n"
    ^ "  Can't spawn this function call.\n"
    ^ (show_source pos)
  in fatal_error err

(** Found an "import" statement inside a function. *)
let err_import_in_function pos fname =
  let err = "At " ^ (format_position pos) ^ ":\n"
    ^ "  Import inside a the function: " ^ fname ^ "\n"
    ^ "  Importing should be done in the global scope-only.\n"
    ^ (show_source pos)
  in fatal_error err

(** Tried to instantiate a template that didn't exist. *)
let err_no_such_template pos name =
  let err = "At " ^ (format_position pos) ^ ":\n"
    ^ "  No such template: " ^ name ^ "\n"
    ^ (show_source pos)
  in fatal_error err

