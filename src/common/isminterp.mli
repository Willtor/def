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
