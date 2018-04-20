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

type fcn_call =
  { fc_pos      : position;
    fc_name     : string;
    fc_template : Parsetree.tokendata list;
    fc_args     : expr list;
    fc_spawn    : bool
  }

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
  | ExprNew of
      position * Types.deftype
      * (*array dimension=*)expr
      * (position * string * (position * expr) option * position * expr) list
  | ExprFcnCall of fcn_call
  | ExprString of position * string
  | ExprBinary of operation
  | ExprPreUnary of operation
  | ExprPostUnary of operation
  | ExprVar of position * string
  | ExprLit of position * literal
  | ExprEnum of position * string * literal * Types.deftype
  | ExprCast of position * Types.deftype * expr
  | ExprIndex of position * expr * position * expr
  | ExprSelectField of position * position * expr * field_id
  | ExprStaticStruct of position * (position * expr) list
  | ExprStaticArray of position * (position * expr) list
  | ExprType of position * Types.deftype
  | ExprTypeString of position * expr
  | ExprNil of position
  | ExprWildcard of position

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
  | DefTemplateFcn of position * Parsetree.pt_template * string option
                      * Types.visibility
                      * string * Types.deftype * stmt list
  | VarDecl of Parsetree.tokendata * Parsetree.tokendata list
               * expr list * Types.deftype * Types.visibility
  | InlineStructVarDecl of Parsetree.tokendata
                           * (position * string * Types.deftype) list
                           * (position * expr)
  | TransactionBlock of position * stmt list
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

val pos_of_astexpr : expr -> position

val provably_always_true : expr -> bool
