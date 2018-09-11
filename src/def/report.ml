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

open Error

(** Report an internal error. *)
let err_internal file line msg =
  let err = "Internal error: file \"" ^ file ^ "\" line "
    ^ (string_of_int line) ^ ".\n"
    ^ msg ^ "\n\n"
    ^ "If you are seeing this error, it indicates a BUG.  For support,\n"
    ^ "contact " ^ Version.project_support ^ " with the error and the\n"
    ^ "following string: " ^ Version.version_build ^ "."
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

(** Couldn't find where clang was installed on the system. *)
let err_unable_to_find_clang () =
  let err = "Unable to find clang installation."
  in fatal_error err

(** Couldn't open file. *)
let err_unable_to_open_file filename =
  let err = "Unable to open file \"" ^ filename ^ "\""
  in fatal_error err

(** Couldn't find the file to open. *)
let err_unable_to_locate_imported_file pos filename =
  err_pos ("Unable to locate file to import: " ^ filename) pos

(** Failed to parse C file. *)
let err_parsing_c () =
  fatal_error "Error(s) parsing C file."

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

(** Syntax error. *)
let err_syntax pos =
  err_pos "Syntax error:" pos

(** The ... in a parameter list did not appear last. *)
let err_vararg_not_last pos =
  err_pos ("The ellipsis denoting a variadic function must appear at the end"
           ^ " of the parameter list.")
          pos

(** Initializing variables: There was a mismatch between the count of
    variables being declared and initializers. *)
let err_var_decl_list_length_mismatch pos vars inits =
  err_pos ("Mismatch between count of variables (" ^ (string_of_int vars)
           ^ ") and initializers (" ^ (string_of_int inits) ^ ").")
          pos

(** Local variable(s) was/were declared "global." *)
let err_global_local_var_decl pos =
  err_pos ("Global variable declaration in a local scope.  Use \"var\" to"
           ^ "declare local variables.")
          pos

(** User specified a variable with no type and no intializer. *)
let err_no_init_on_inferred_type pos =
  err_pos "Cannot infer variable type without an initializer." pos

let err_multiple_volatile_keywords pos =
  err_pos ("Keyword 'volatile' cannot be specified multiple times.  "
           ^ "This is probably a typo.")
          pos

(** Function has no return statement. *)
let err_no_return pos fcn =
  err_pos ("Function " ^ fcn ^ " needs a return statement.") pos

(** Local function was declared "export" which is not allowed. *)
let err_local_fcn_with_nonlocal_vis pos fcn =
  err_pos ("Function " ^ fcn ^ " has non-global scope, and must have local "
           ^ "visibility.")
          pos

(** Used an undefined variable. *)
let err_undefined_var pos name =
  err_pos ("Variable " ^ name ^ " is undefined.") pos

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
  err_pos ("Unknown function \"" ^ name ^ "\".") pos

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

(** Called a builtin function with the wrong number of args. *)
let err_wrong_number_of_builtin_args pos name n_args =
  err_pos ("Builtin function " ^ name ^ " called with "
           ^ (string_of_int n_args) ^ " arguments.")
          pos

(** Report that the type of the first argument to the function should be a
    jump buffer. *)
let err_first_arg_should_be_jumpbuf pos name =
  err_pos ("The first argument to " ^ name
           ^ " should be a jumpbuf ([20]*void).")
          pos

(** This is just how __builtin_longjmp works, y'all. *)
let err_builtin_longjmp_wants_literal_one pos =
  err_pos "__builtin_longjmp's second argument should be a literal 1."
          pos

(** Tried to perform an atomic operation on a non-integer, non-pointer type. *)
let err_atomic_non_integer pos typename =
  err_pos ("Cannot perform an atomic operation on type " ^ typename ^ ".") pos

(** Called an atomic function, and the dest was not a pointer. *)
let err_atomic_dest_not_ptr pos name =
  err_pos ("Atomic function " ^ name
           ^ " takes a pointer as its first argument.")
          pos

(** parfor was written without an iteration expression.  It requires this since
    this is the only thing that happens outside the parallel region. *)
let err_parfor_needs_iter pos =
  err_pos "A parfor requires an iteration expression." pos

(** Tried to call a variable that was not a function. *)
let err_called_non_fcn use_pos decl_pos name =
  let err = "At " ^ (format_position use_pos) ^ ":\n"
    ^ "  Called " ^ name ^ ", which was not declared as a function.\n"
    ^ (show_source use_pos) ^ "\n"
    ^ "  Declared at " ^ (format_position decl_pos) ^ "\n"
    ^ (show_source decl_pos)
  in fatal_error err

(** Declared a new variable with the same name as another one in the same
    scope. *)
let err_redeclared_variable pos orig_pos var =
  let err = "At " ^ (format_position pos) ^ ":\n"
    ^ "  Redeclared variable \"" ^ var ^ "\".\n"
    ^ (show_source pos) ^ "\n"
    ^ "  Originally declared at " ^ (format_position orig_pos) ^ "\n"
    ^ (show_source orig_pos)
  in fatal_error err

(** User specified an unknown or undeclared type. *)
let err_unknown_typename pos name =
  err_pos ("Unknown type name: " ^ name ^ ".") pos

(** Returned void in a non-void function.
    FIXME: This msg could be better. *)
let err_returned_void pos =
  err_pos "Returned void in a non-void function." pos

(** Arrays can only have dimensions expressed in integers. *)
let err_float_array_dim pos =
  err_pos "Array has dimension of type float." pos

(** Couldn't resolve the array dimension. *)
let err_cant_resolve_array_dim pos =
  err_pos "Unable to statically resolve the dimension of the array." pos

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
  err_pos ("Modulo operator (%) can only be performed on integer types,"
           ^ " but the type here is " ^ ltype ^ " % " ^ rtype ^ ".")
          pos

(** Tried to index a pointer with a non-integer index.
    FIXME: Error message should include type of the index. *)
let err_non_integer_index pos =
  err_pos "The index is not an integer." pos

(** Indexed a non-pointer.
    FIXME: Error message should include type of base. *)
let err_index_non_ptr pos =
  err_pos "Can't index into something that isn't a pointer." pos

(** Type contains itself -- not as a pointer, but as the raw type. *)
let err_recursive_type_defn pos name =
  err_pos ("Recursive defintion of type " ^ name ^ ".") pos

let err_struct_not_enough_fields pos n =
  err_pos ("Field number " ^ (string_of_int n) ^ " is too high."
           ^ "  Note: Field numbers are zero-indexed.")
          pos

(** Tried to access a struct member that doesn't exist in the struct. *)
let err_struct_no_such_member pos mname =
  err_pos ("Struct has no such member: " ^ mname ^ ".") pos

(** Tried to access a member of an object that was not a struct.
    FIXME: Should name the type. *)
let err_non_struct_member_access pos =
  err_pos "Object is not a struct and has no members to access." pos

(** Expression's type doesn't match the expected type.
    FIXME: Should identify the two types. *)
let err_type_mismatch pos has expected =
  err_pos
    ("Type mismatch:\n    Found: " ^ has ^ "\n    Expected: " ^ expected)
    pos

(** Type expression in unexpected (disallowed) location. *)
let err_unexpected_type_expr pos =
  err_pos "Unexpected type expression." pos

(** Builtin function doesn't allow the given format. *)
let err_bad_args_for_builtin pos builtin_fcn =
  err_pos ("Format not allowed for builtin function: " ^ builtin_fcn) pos

(** Tried to spawn internally in an expression or in a condition or some
    such disallowed place. *)
let err_bad_spawn_loc pos =
  err_pos "Can't spawn this function call." pos

(** Found an "import" statement inside a function. *)
let err_import_in_function pos fname =
  err_pos ("Import inside a the function: " ^ fname ^ ".  "
           ^ "Importing should be done in the global scope-only.")
          pos

(** Tried to instantiate a template that didn't exist. *)
let err_no_such_template pos name =
  err_pos ("No such template: " ^ name) pos

(** Error trying to find the more general of two types. *)
let err_generalizing_types pos t1 t2 =
  err_pos ("Unable to reconcile types " ^ t1 ^ " and " ^ t2 ^ ".") pos

(** Invalid cast. *)
let err_cant_cast pos t1 t2 =
  err_pos ("Can't cast " ^ t1 ^ " to " ^ t2 ^ ".") pos

(** Implicit cast fail. *)
let err_need_explicit_cast pos t1 t2 =
  err_pos ("Need an explicit cast from " ^ t1 ^ " to " ^ t2 ^ ".") pos

(** Tried to "break;" out of... nothing. *)
let err_no_break_scope pos =
  err_pos "There is no construct that can be broken out of." pos

(** Warn there is a loss of precision in an implicit conversion. *)
let warn_loss_of_precision pos conv_str =
  warn_pos ("Loss of precision: " ^ conv_str) pos

(** Volatility on the source type, but not on the target type. *)
let warn_implicit_loss_of_volatility pos =
  warn_pos "Implicit cast loses volatility." pos
