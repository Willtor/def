(** Return the default set of bindings. *)
val bindings_create : unit -> Parsetree.binding Util.symtab

(** Interpret a STU expression and return the result. *)
val eval_stu : Parsetree.binding Util.symtab
               -> Parsetree.stu
               -> Parsetree.stu
