
open Lexing
open Types

type literal =
  | LitBool of Lexing.position * bool
  (* schar, uchar *)
  | LitI16 of Lexing.position * int32 
  | LitU16 of Lexing.position * int32
  | LitI32 of Lexing.position * int32
  | LitU32 of Lexing.position * int32
  | LitI64 of Lexing.position * int64
  | LitU64 of Lexing.position * int64
  (* floating point *)

type operator =
  | OperIncr of position | OperDecr of position
  | OperMinus of position | OperPlus of position
  | OperLogicalNot of position | OperBitwiseNot of position
  | OperMult of position | OperDiv of position | OperRemainder of position
  | OperLShift of position | OperRShift of position
  | OperLT of position | OperGT of position | OperLTE of position
  | OperGTE of position | OperEquals of position | OperNEquals of position
  | OperBitwiseAnd of position | OperBitwiseXor of position
  | OperBitwiseOr of position
  | OperLogicalAnd of position | OperLogicalOr of position
  | OperAssign of position | OperPlusAssign of position
  | OperMinusAssign of position | OperMultAssign of position
  | OperDivAssign of position | OperRemAssign of position
  | OperLShiftAssign of position | OperRShiftAssign of position
  | OperBAndAssign of position
  | OperBXorAssign of position | OperBOrAssign of position

type vartype =
  | VarType of position * string
  | FcnType of (position * string * vartype) list * vartype
  | PtrType of position * vartype

type expr =
  | ExprFcnCall of position * string * expr list
  | ExprString of position * string
  | ExprBinary of operator * expr * expr
  | ExprPreUnary of operator * expr
  | ExprPostUnary of operator * expr
  | ExprVar of position * string
  | ExprLit of literal
  | ExprCast of position * vartype * expr
  | ExprIndex of position * expr * position * expr

type stmt =
  | StmtExpr of position * expr
  | Block of position * stmt list
  | DefFcn of position * string * vartype * stmt list
  | VarDecl of position * string * vartype * (position * expr) option
  | IfStmt of position * expr * stmt list * stmt list option
  | WhileLoop of position * expr * stmt list
  | Return of position * expr
  | ReturnVoid of position

let operator2string = function
  | OperIncr _ -> "++"
  | OperDecr _ -> "--"
  | OperMinus _ -> "-"
  | OperPlus _ -> "+"
  | OperLogicalNot _ -> "!"
  | OperBitwiseNot _ -> "~"
  | OperMult _ -> "*"
  | OperDiv _ -> "/"
  | OperRemainder _ -> "%"
  | OperLShift _ -> "<<"
  | OperRShift _ -> ">>"
  | OperLT _ -> "<"
  | OperGT _ -> ">"
  | OperLTE _ -> "<="
  | OperGTE _ -> ">="
  | OperEquals _ -> "=="
  | OperNEquals _ -> "!="
  | OperBitwiseAnd _ -> "&"
  | OperBitwiseXor _ -> "^"
  | OperBitwiseOr _ -> "|"
  | OperLogicalAnd _ -> "&&"
  | OperLogicalOr _ -> "||"
  | OperAssign _ -> "="
  | OperPlusAssign _ -> "+="
  | OperMinusAssign _ -> "-="
  | OperMultAssign _ -> "*="
  | OperDivAssign _ -> "/="
  | OperRemAssign _ -> "%="
  | OperLShiftAssign _ -> "<<="
  | OperRShiftAssign _ -> ">>="
  | OperBAndAssign _ -> "&="
  | OperBXorAssign _ -> "^="
  | OperBOrAssign _ -> "|="

let literal2primitive = function
  | LitBool _ -> PrimBool
  | LitI16 _ -> PrimI16
  | LitU16 _ -> PrimU16
  | LitI32 _ -> PrimI32
  | LitU32 _ -> PrimU32
  | LitI64 _ -> PrimI64
  | LitU64 _ -> PrimU64
