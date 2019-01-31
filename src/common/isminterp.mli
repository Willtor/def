(** Return the length of an S-expression.  If the arg isn't an S-expr, an
    error is generated. *)
val sexpr_length : Parsetree.ism -> int

(** From the given fcn-name's parameters return the single argument (or
    report an error otherwise). *)
val ism_extract_single_arg : string -> Parsetree.ism -> Parsetree.ism

(** Return the default set of bindings. *)
val bindings_create : unit -> Parsetree.binding Util.symtab

(** Interpret a STU expression and return the result. *)
val eval_ism : Parsetree.binding Util.symtab
               -> Parsetree.ism
               -> Parsetree.ism

(** Return a token from an ident.  This will generate an internal error
    if the ident is still an unevaluated ISM. *)
val tok_of_ident : Parsetree.binding Util.symtab
                   -> Parsetree.ident
                   -> Parsetree.tokendata
