
type atom =
  | AtomInt of int

type expr =
  | ExprBinary of string * expr * expr
  | ExprPreUnary of string * expr
  | ExprPostUnary of string * expr
  | ExprAtom of atom

type stmt =
  | StmtExpr of expr

let atom2string = function
  | AtomInt i -> (string_of_int i)

let rec expr2string = function
  | ExprBinary (op, e1, e2) ->
     "(" ^ op ^ " " ^ (expr2string e1) ^ " " ^ (expr2string e2) ^ ")"
  | ExprPreUnary (op, e) ->
     "(pre " ^ op ^ " " ^ (expr2string e) ^ ")"
  | ExprPostUnary (op, e) ->
     "(post " ^ op ^ " " ^ (expr2string e) ^ ")"
  | ExprAtom a ->
     atom2string a

let stmt2string = function
  | StmtExpr e -> "Stmt: " ^ (expr2string e)
