
type operator =
  | OperIncr | OperDecr
  | OperMinus | OperPlus
  | OperLogicalNot | OperBitwiseNot
  | OperMult | OperDiv | OperRemainder
  | OperLShift | OperRShift
  | OperLT | OperGT | OperLTE | OperGTE | OperEquals
  | OperBitwiseAnd | OperBitwiseXor | OperBitwiseOr
  | OperLogicalAnd | OperLogicalOr
  | OperAssign | OperPlusAssign | OperMinusAssign
  | OperMultAssign | OperDivAssign | OperRemAssign
  | OperLShiftAssign | OperRShiftAssign | OperBAndAssign
  | OperBXorAssign | OperBOrAssign

type atom =
  | AtomInt of int

type expr =
  | ExprBinary of operator * expr * expr
  | ExprPreUnary of operator * expr
  | ExprPostUnary of operator * expr
  | ExprAtom of atom

type stmt =
  | StmtExpr of expr

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
  | OperEquals -> "="
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

let atom2string = function
  | AtomInt i -> (string_of_int i)

let rec expr2string = function
  | ExprBinary (op, e1, e2) ->
     "(" ^ (operator2string op) ^ " " ^ (expr2string e1)
     ^ " " ^ (expr2string e2) ^ ")"
  | ExprPreUnary (op, e) ->
     "(pre " ^ (operator2string op) ^ " " ^ (expr2string e) ^ ")"
  | ExprPostUnary (op, e) ->
     "(post " ^ (operator2string op) ^ " " ^ (expr2string e) ^ ")"
  | ExprAtom a ->
     atom2string a

let stmt2string = function
  | StmtExpr e -> "Stmt: " ^ (expr2string e)
