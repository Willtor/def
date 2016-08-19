
open Lexing

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

type atom =
  | AtomInt of position * int
  | AtomVar of position * string

type vartype =
  | VarType of position * string
  | FcnType of (position * string * vartype) list * vartype

type expr =
  | ExprFcnCall of position * string * expr list
  | ExprString of position * string
  | ExprBinary of operator * expr * expr
  | ExprPreUnary of operator * expr
  | ExprPostUnary of operator * expr
  | ExprAtom of atom
  | ExprCast of position * vartype * expr

type stmt =
  | StmtExpr of expr
  | Block of stmt list
  | DefFcn of position * string * vartype * stmt
  | IfStmt of expr * stmt list * stmt list option
  | Return of expr
  | ReturnVoid

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

let atom2string = function
  | AtomInt (_, i) -> (string_of_int i)
  | AtomVar (_, s) -> s

let rec vartype2string = function
  | VarType (_, s) -> s
  | FcnType (args, ret) ->
     "("
     ^ (List.fold_left
          (fun s (_, n, e) ->
            s ^ (vartype2string e) ^ " " ^ n ^ ", ") "" args)
     ^ ") -> " ^ (vartype2string ret)

let rec expr2string = function
  | ExprFcnCall (_, nm, elist) ->
     let rec printlist = function
       | [] -> ""
       | [e] -> expr2string e
       | e :: rest -> (expr2string e) ^ ", " ^ (printlist rest)
     in nm ^ "(" ^ (printlist elist) ^ ")"
  | ExprString (_, str) -> str
  | ExprBinary (op, e1, e2) ->
     "(" ^ (operator2string op) ^ " " ^ (expr2string e1)
     ^ " " ^ (expr2string e2) ^ ")"
  | ExprPreUnary (op, e) ->
     "(pre " ^ (operator2string op) ^ " " ^ (expr2string e) ^ ")"
  | ExprPostUnary (op, e) ->
     "(post " ^ (operator2string op) ^ " " ^ (expr2string e) ^ ")"
  | ExprAtom a ->
     atom2string a
  | ExprCast (_, vt, e) ->
     "(" ^ (vartype2string vt) ^ ")" ^ (expr2string e)

let rec plist2string = function
  | [] -> ""
  | [(_, nm, t)] -> nm ^ ": " ^ (vartype2string t)
  | (_, nm, t) :: rest -> nm ^ ": " ^ (vartype2string t) ^ ", "
     ^ (plist2string rest)

let rec stmt2string = function
  | StmtExpr e -> "StmtExpr: " ^ (expr2string e) ^ "\n"
  | Block slist -> "Block: [\n" ^
     (List.fold_left (fun s stmt -> s ^ (stmt2string stmt)) "" slist) ^ "]\n"
  | DefFcn (_, name, tp, s) ->
     "def " ^ name ^ (vartype2string tp) ^ "\n" ^ (stmt2string s)
  | IfStmt _ ->
     "if stmt (not fully unparsed).\n"
  | Return e ->
     "return " ^ (expr2string e) ^ "\n"
  | ReturnVoid ->
     "return\n"
