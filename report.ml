open Ast
open Util

(** Report an internal error. *)
let err_internal file line msg =
  let err = "Internal error: file \"" ^ file ^ "\" line "
    ^ (string_of_int line) ^ ".\n"
    ^ msg
  in fatal_error err

(** Lexing error. *)
let err_lexing pos character =
  let err = "At " ^ (format_position pos) ^ ":\n"
    ^ "  Unexpected character: " ^ character ^ "\n"
    ^ (show_source pos)
  in fatal_error err

(** Error: The operator was invalid for the type being operated on. *)
let err_invalid_op pos op tpname =
  let err = "At " ^ (format_position pos) ^ ":\n"
    ^ "  Can't use operator " ^ (operator2string op) ^ " on type "
    ^ tpname ^ ".\n"
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
