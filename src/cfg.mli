type cfg_expr =
  | Expr_FcnCall of string * cfg_expr list
  | Expr_Binary of Ast.operator * Types.deftype * cfg_expr * cfg_expr
  | Expr_Unary of Ast.operator * Types.deftype * cfg_expr * bool
  | Expr_Literal of Ast.literal
  | Expr_Variable of string
  | Expr_Cast of Types.deftype * Types.deftype * cfg_expr
  | Expr_Index of cfg_expr * cfg_expr
  | Expr_SelectField of cfg_expr * int
  | Expr_StaticStruct of (Types.deftype * cfg_expr) list
  | Expr_Nil
  | Expr_Atomic of atomic_op * (Types.deftype * cfg_expr) list

and atomic_op =
  | AtomicCAS

type cfg_basic_block =
  | BB_Cond of string * conditional_block
  | BB_Loop of string * loop_block
  | BB_Expr of Lexing.position * string * cfg_expr
  | BB_Return of Lexing.position * cfg_expr
  | BB_ReturnVoid of Lexing.position
  | BB_LocalFcn of function_defn
  | BB_Label of string
  | BB_Goto of string
  | BB_Continue

and conditional_block =
  { if_pos       : Lexing.position;
    fi_pos       : Lexing.position;
    branch_cond  : cfg_expr;

    mutable then_scope : cfg_basic_block list;
    then_has_exit : bool;

    mutable else_scope : cfg_basic_block list;
    else_has_exit : bool
  }

and loop_block =
  { while_pos  : Lexing.position;
    precheck   : bool;
    loop_cond  : cfg_expr;
    mutable body_scope : cfg_basic_block list;
    can_exit   : bool;
  }

and decl =
  { decl_pos   : Lexing.position;
    mappedname : string;
    vis        : Types.visibility;
    tp         : Types.deftype;
    params     : (Lexing.position * string) list (* Zero-length for non-fcns *)
  }

and function_defn =
  { defn_begin : Lexing.position;
    defn_end   : Lexing.position;
    name       : string;
    local_vars : (string * decl) list;
    mutable bbs : cfg_basic_block list;
  }

and program =
  { global_decls : decl Util.symtab;
    fcnlist : function_defn list;
    deftypemap : Types.deftype Util.symtab
  }

val convert_ast : Ast.stmt list -> program
