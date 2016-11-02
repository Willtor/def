type cfg_expr =
  | Expr_FcnCall of string * cfg_expr list
  | Expr_Binary of Ast.operator * Types.deftype * cfg_expr * cfg_expr
  | Expr_Unary of Ast.operator * Types.deftype * cfg_expr * bool
  | Expr_Literal of Ast.literal
  | Expr_Variable of string
  | Expr_Cast of Types.deftype * Types.deftype * cfg_expr
  | Expr_Index of cfg_expr * cfg_expr
  | Expr_SelectField of cfg_expr * int

type cfg_basic_block =
  | BB_Cond of conditional_block
  | BB_Loop of loop_block
  | BB_Expr of Lexing.position * cfg_expr
  | BB_Return of Lexing.position * cfg_expr
  | BB_ReturnVoid of Lexing.position

and conditional_block =
  { if_pos       : Lexing.position;
    fi_pos       : Lexing.position;
    branch_cond  : cfg_expr;

    then_scope   : cfg_basic_block list;
    then_returns : bool;

    else_scope   : cfg_basic_block list;
    else_returns : bool
  }

and loop_block =
  { while_pos  : Lexing.position;
    precheck   : bool;
    loop_cond  : cfg_expr;
    body_scope : cfg_basic_block list;
  }

and decl =
  { decl_pos   : Lexing.position;
    mappedname : string;
    tp         : Types.deftype;
    params     : (Lexing.position * string) list (* Zero-length for non-fcns *)
  }

type function_defn =
  { defn_begin : Lexing.position;
    defn_end   : Lexing.position;
    name       : string;
    local_vars : (string * decl) list;
    bbs        : cfg_basic_block list
  }

type program =
  { global_decls : decl Util.symtab;
    fcnlist : function_defn list;
    deftypemap : Types.deftype Util.symtab
  }

val convert_ast : Ast.stmt list -> program
