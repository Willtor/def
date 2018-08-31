type position = Lexing.position

type literal =
  | LitBool of bool
  | LitI8  of char
  | LitU8  of char
  | LitI16 of int32
  | LitU16 of int32
  | LitI32 of int32
  | LitU32 of int32
  | LitI64 of int64
  | LitU64 of int64
  | LitF32 of float
  | LitF64 of float

type code_relation =
  | CRExpr of Parsetree.pt_expr
  | CRApproximate of position

and operation =
  { op_pos : position;
    op_op : Operator.t;
    op_left : expr;
    op_right : expr option;
    op_atomic : bool
  }

and field_id =
  | FieldNumber of int
  | FieldName of string

and expr =
  { expr_cr   : code_relation;
    expr_tp   : Types.deftype;
    expr_ast  : ast_expr
  }

and ast_expr =
  | ExprNew of (*array dim=*)expr * Types.deftype
               * (Parsetree.tokendata * expr) list
  | ExprFcnCall of string * expr list * (*spawn=*)bool
  | ExprString of string
  | ExprBinary of operation
  | ExprPreUnary of operation
  | ExprPostUnary of operation
  | ExprVar of string
  | ExprLit of literal
  | ExprEnum of string * literal
  | ExprCast of (*from=*)Types.deftype * (*to=*)Types.deftype * expr
  | ExprIndex of (*base=*)expr * (*idx=*)expr
  | ExprSelectField of expr * field_id
  | ExprStaticStruct of (*is_packed=*)bool * expr list
  | ExprStaticArray of expr list
  | ExprType of Types.deftype
  | ExprTypeString of expr
  | ExprNil
  | ExprWildcard

type stmt =
  | Import of Parsetree.tokendata * Parsetree.tokendata
  | StmtExpr of position * expr
  | Block of position * stmt list
  | DeclFcn of position * Types.visibility * string * Types.deftype
               * (position * string) list
  | DefFcn of position * string option * Types.visibility * string
              * Types.deftype
              * (position * string) list
              * stmt list
  | VarDecl of Parsetree.tokendata * Parsetree.tokendata list
               * expr list * Types.deftype * Types.visibility
  | InlineStructVarDecl of Parsetree.tokendata
                           * (position * string * Types.deftype) list
                           * (position * expr)
  | TransactionBlock of position * stmt list * (position * stmt list) option
  | IfStmt of position * expr * stmt list * (position * stmt list) option
  (* ForLoop: start-pos * is_parallel * init * cond * iter * body *)
  | ForLoop of position * bool * stmt option * (position * expr)
    * (position * expr) option * position * stmt list
  (* WhileLoop: start-pos * pre-check * cond * body *)
  | WhileLoop of position * bool * expr * stmt list
  (* SwitchStmt:
     start-pos * expr * (case-pos * fall-through? * expr * body) list *)
  | SwitchStmt of position * expr * (position * bool * expr * stmt list) list
  | Return of position * expr
  | ReturnVoid of position
  | TypeDecl of position * string * Types.deftype * Types.visibility * bool
  | Label of position * string
  | Goto of position * string
  | Break of position
  | Continue of position
  | Sync of position

val of_parsetree : Parsetree.pt_stmt list -> stmt list

val of_cimport : Cimportext.cvalue list -> stmt list

val literal2primitive : literal -> Types.primitive

val pos_of_cr : code_relation -> position

val pos_of_astexpr : expr -> position

val provably_always_true : expr -> bool
