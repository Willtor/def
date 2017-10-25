
(** Mangle a name with template parameters. *)
val mangle : string -> Parsetree.tokendata list -> string

(** Statically expand all templates as required and throw away the templates,
    themselves. *)
val expand : Ast.stmt list -> Ast.stmt list
