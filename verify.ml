open Ast

let verify_expr e = e


let verify_ast =
  let rec verify accum = function
    | stmt :: rest ->
       let rec verify_stmt = function
         | StmtExpr e ->
            let verified_e = verify_expr e in StmtExpr verified_e
         | Block stmt_list -> 
            let verified_stmt_list = verify [] stmt_list
            in Block verified_stmt_list
         | DefFcn (fn, stmt) ->
            let verified_fn = fn in
            let verified_stmt = verify_stmt stmt in
            DefFcn (verified_fn, verified_stmt)
         | Return e ->
            let verified_e = verify_expr e in Return verified_e
       in
       verify (verify_stmt stmt :: accum) rest
    | [] -> List.rev accum
  in verify []

