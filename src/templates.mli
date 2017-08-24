
(** Statically expand all templates as required and throw away the templates,
    themselves. *)
val expand : Ast.stmt list -> Ast.stmt list
