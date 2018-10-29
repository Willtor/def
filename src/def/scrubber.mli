type program =
  { prog_typemap : Types.deftype Util.symtab;
    prog_ast     : Ast.stmt list
  }

val scrub : Parsetree.binding Util.symtab * Ast.stmt list -> program
