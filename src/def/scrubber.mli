type program =
  { prog_typemap : Types.deftype Util.symtab;
    prog_ast     : Ast.stmt list
  }

val scrub : Ast.stmt list -> program
