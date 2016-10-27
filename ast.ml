
open Types
open Lexing

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
  (* floating point *)

type operator =
  | OperIncr | OperDecr
  | OperMinus | OperPlus
  | OperLogicalNot | OperBitwiseNot
  | OperMult | OperDiv | OperRemainder
  | OperLShift | OperRShift
  | OperLT | OperGT | OperLTE
  | OperGTE | OperEquals | OperNEquals
  | OperBitwiseAnd | OperBitwiseXor | OperBitwiseOr
  | OperLogicalAnd | OperLogicalOr
  | OperAssign | OperPlusAssign
  | OperMinusAssign | OperMultAssign
  | OperDivAssign | OperRemAssign
  | OperLShiftAssign | OperRShiftAssign
  | OperBAndAssign | OperBXorAssign | OperBOrAssign

type vartype =
  | VarType of position * string
  | FcnType of (position * string * vartype) list * vartype
  | StructType of (position * string * vartype) list
  | PtrType of position * vartype

type fcn_call =
  { fc_pos  : position;
    fc_name : string;
    fc_args : expr list
  }

and operation =
  { op_pos : position;
    op_op : operator;
    op_left : expr;
    op_right : expr option
  }

and expr =
  | ExprFcnCall of fcn_call
  | ExprString of position * string
  | ExprBinary of operation
  | ExprPreUnary of operation
  | ExprPostUnary of operation
  | ExprVar of position * string
  | ExprLit of position * literal
  | ExprCast of position * vartype * expr
  | ExprIndex of position * expr * position * expr
  | ExprSelectField of position * position * expr * string

type stmt =
  | StmtExpr of position * expr
  | Block of position * stmt list
  | DefFcn of position * string * vartype * stmt list
  | VarDecl of position * string * vartype * (position * expr) option
  | IfStmt of position * expr * stmt list * stmt list option
  (* WhileLoop: start-pos * pre-check * cond * body *)
  | WhileLoop of position * bool * expr * stmt list
  | Return of position * expr
  | ReturnVoid of position
  | TypeDecl of position * string * vartype

let operator2string = function
  | OperIncr -> "++"
  | OperDecr -> "--"
  | OperMinus -> "-"
  | OperPlus -> "+"
  | OperLogicalNot -> "!"
  | OperBitwiseNot -> "~"
  | OperMult -> "*"
  | OperDiv -> "/"
  | OperRemainder -> "%"
  | OperLShift -> "<<"
  | OperRShift -> ">>"
  | OperLT -> "<"
  | OperGT -> ">"
  | OperLTE -> "<="
  | OperGTE -> ">="
  | OperEquals -> "=="
  | OperNEquals -> "!="
  | OperBitwiseAnd -> "&"
  | OperBitwiseXor -> "^"
  | OperBitwiseOr -> "|"
  | OperLogicalAnd -> "&&"
  | OperLogicalOr -> "||"
  | OperAssign -> "="
  | OperPlusAssign -> "+="
  | OperMinusAssign -> "-="
  | OperMultAssign -> "*="
  | OperDivAssign -> "/="
  | OperRemAssign -> "%="
  | OperLShiftAssign -> "<<="
  | OperRShiftAssign -> ">>="
  | OperBAndAssign -> "&="
  | OperBXorAssign -> "^="
  | OperBOrAssign -> "|="

let literal2primitive = function
  | LitBool _ -> PrimBool
  | LitI8  _ -> PrimI8
  | LitU8  _ -> PrimU8
  | LitI16 _ -> PrimI16
  | LitU16 _ -> PrimU16
  | LitI32 _ -> PrimI32
  | LitU32 _ -> PrimU32
  | LitI64 _ -> PrimI64
  | LitU64 _ -> PrimU64
