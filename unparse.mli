val depthify : int -> string -> string
val unparse_stmt : int -> Ast.stmt -> unit
val unparse_deffcn : int -> Ast.deffcn -> unit
val unparse_type : string -> Ast.vartype -> string
val unparse_expr_wrapper : Ast.expr -> unit
val unparse_expr : Ast.expr -> unit
val unparse : Ast.stmt list -> unit
