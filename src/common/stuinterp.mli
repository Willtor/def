val stu_builtins :
  (string * (Lexing.position -> Parsetree.stu list -> Parsetree.stu)) list

val eval_stu : Parsetree.binding Util.symtab
               -> Parsetree.stu
               -> Parsetree.stu
