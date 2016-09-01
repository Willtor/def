open Ast
open Util

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
