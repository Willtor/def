open Ast
open Util

exception NoReturn

type term_condition =
  | NOT_TERMINATED
  | TERMINATED
  | TERMINATED_AND_REPORTED

let position_of_stmt = function
  | StmtExpr (pos, _)
  | Block (pos, _)
  | DefFcn (pos, _, _, _)
  | VarDecl (pos, _, _, _)
  | IfStmt (pos, _, _, _)
  | WhileLoop (pos, _, _)
  | Return (pos, _)
  | ReturnVoid pos -> pos

let kill_dead_code =
  let report_dead_code fcn pos =
    let warn = "Dead code in function \"" ^ fcn ^ "\": "
      ^ (format_position pos) ^ "\n" ^ (show_source pos)
    in warning warn
  in
  let rec process name =
    let rec proc accum = function
      | [] -> List.rev accum
      | StmtExpr _ as stmt :: rest ->
         proc (stmt :: accum) rest
      | Block (pos, slist) :: rest ->
         let stmt = Block (pos, proc [] slist) in
         proc (stmt :: accum) rest
      | DefFcn (pos, name, tp, body) :: rest ->
         let stmt = DefFcn (pos, name, tp, process name body) in
         proc (stmt :: accum) rest
      | VarDecl _ as stmt :: rest ->
         proc (stmt :: accum) rest
      | IfStmt (pos, cond, thenblk, maybe_else) :: rest ->
         let stmt = IfStmt (pos, cond, proc [] thenblk,
                            match maybe_else with
                            | None -> None
                            | Some elseblk -> Some (proc [] elseblk))
         in proc (stmt :: accum) rest
      | WhileLoop (pos, cond, body) :: rest ->
         let stmt = WhileLoop (pos, cond, proc [] body)
         in proc (stmt :: accum) rest
      | (Return _ as stmt) :: rest | (ReturnVoid _ as stmt) :: rest ->
         let () = match rest with
           | [] -> ()
           | extra :: rest -> report_dead_code name (position_of_stmt extra)
         in List.rev (stmt :: accum)
    in proc []
  in
  let toplevel = function
    | DefFcn (pos, name, tp, body) ->
       DefFcn (pos, name, tp, process name body)
    | stmt -> stmt
  in List.map toplevel

let scrub stmts =
  let stmts = kill_dead_code stmts in
  stmts
