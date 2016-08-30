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
