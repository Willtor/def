type cfg_basic_block =
  | BB_Cond of conditional_block
  | BB_Expr of Lexing.position * Ast.expr
  | BB_Scope of cfg_scope
  | BB_Return of Lexing.position * Ast.expr
  | BB_ReturnVoid of Lexing.position

and cfg_scope =
  { local_vars : (string * decl) list;
    bbs : cfg_basic_block list
  }

and conditional_block =
  { if_pos       : Lexing.position;
    fi_pos       : Lexing.position;
    cond         : Ast.expr;

    then_scope   : cfg_scope;
    then_returns : bool;

    else_scope   : cfg_scope;
    else_returns : bool
  }

and decl =
  { decl_pos : Lexing.position;
    declname : string;
    tp       : Ast.vartype
  }

type function_defn =
  { defn_begin : Lexing.position;
    defn_end   : Lexing.position;
    body       : cfg_scope
  }

type program =
  { global_decls : decl Util.symtab;
    fcnlist : function_defn list
  }

val convert_ast : Ast.stmt list -> program
