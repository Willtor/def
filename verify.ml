open Ast

let verify_stmt = function
  | StmtExpr e -> StmtExpr e
  | Block stmt_list -> Block stmt_list
  | DefFcn (fn, stmt) -> DefFcn (fn, stmt)
  | Return e -> Return e

let verify_ast =
  let rec verify accum = function
    | stmt :: rest ->
       verify (verify_stmt stmt :: accum) rest
    | [] -> List.rev accum
  in verify []
