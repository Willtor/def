
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
  | AtomVar of string

type expr =
  | ExprBinary of operator * expr * expr
  | ExprPreUnary of operator * expr
  | ExprPostUnary of operator * expr
  | ExprAtom of atom

type vartype =
  | VarType of string

type deffcn =
  | NamedFunction of string * (string * vartype) list * vartype

type stmt =
  | StmtExpr of expr
  | Block of stmt list
  | DefFcn of deffcn * stmt
  | Return of expr

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
  | AtomVar s -> s

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

let vartype2string = function
  | VarType s -> s

let rec plist2string = function
  | [] -> ""
  | [(nm, t)] -> nm ^ ": " ^ (vartype2string t)
  | (nm, t) :: rest -> nm ^ ": " ^ (vartype2string t) ^ ", "
     ^ (plist2string rest)

let deffcn2string = function
  | NamedFunction (nm, plist, ret) ->
     "function " ^ nm ^ "{" ^ (plist2string  plist) ^ "} -> "
     ^ (vartype2string ret)

let rec stmt2string = function
  | StmtExpr e -> "StmtExpr: " ^ (expr2string e) ^ "\n"
  | Block slist -> "Block: [\n" ^
     (List.fold_left (fun s stmt -> s ^ (stmt2string stmt)) "" slist) ^ "]\n"
  | DefFcn (f, s) ->
     "def" ^ (deffcn2string f) ^ "\n" ^ (stmt2string s)
  | Return e ->
     "return " ^ (expr2string e) ^ "\n"
