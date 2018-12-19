(** Return the default set of bindings. *)
val bindings_create : unit -> Parsetree.binding Util.symtab

(** Interpret a STU expression and return the result. *)
val eval_ism : Parsetree.binding Util.symtab
               -> Parsetree.ism
               -> Parsetree.ism
