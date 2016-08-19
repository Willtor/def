open Ast
open Util

exception NoReturn

type term_condition =
  | NOT_TERMINATED
  | TERMINATED
  | TERMINATED_AND_REPORTED

let all_paths_return =
  let check_body tp body =
    let retvoid = match tp with
      | VarType (_, "void") -> true
      | _ -> false
    in
    let report_dead_code () =
      warning "Dead code in function.  FIXME: Won't tell you which.";
      TERMINATED_AND_REPORTED
    in
    let rec check term_p = function
      | Return _ | ReturnVoid ->
         begin match term_p with
         | NOT_TERMINATED -> TERMINATED
         | TERMINATED -> report_dead_code ()
         | TERMINATED_AND_REPORTED -> TERMINATED_AND_REPORTED
         end
      | IfStmt (_, then_stmts, Some else_stmts) ->
         let then_term = List.fold_left check term_p then_stmts
         and else_term = List.fold_left check term_p else_stmts
         in begin match then_term, else_term with
         | NOT_TERMINATED, _ | _, NOT_TERMINATED -> NOT_TERMINATED
         | TERMINATED_AND_REPORTED, _ | _, TERMINATED_AND_REPORTED ->
            TERMINATED_AND_REPORTED
         | _ -> TERMINATED
         end
      | Block stmts ->
         begin match List.fold_left check term_p stmts with
         | TERMINATED -> report_dead_code ()
         | status -> status
         end
      | _ ->
         begin match term_p with
         | TERMINATED -> report_dead_code ()
         | status -> status
         end
    in
    match body with
    | StmtExpr e ->
       if retvoid then StmtExpr e
       else Return e
    | Block stmts ->
       begin match List.fold_left check NOT_TERMINATED stmts with
       | NOT_TERMINATED ->
          if retvoid then Block (List.append stmts [ReturnVoid])
          else raise NoReturn
       | TERMINATED | TERMINATED_AND_REPORTED -> Block stmts
       end
    | _ ->
       fatal_error
         "Internal error: all_paths_return: Unexpected function body."
  in
  let check_fcn = function
    | DefFcn (pos, name, tp, body) ->
       begin try DefFcn (pos, name, tp, (check_body tp body))
         with NoReturn ->
           let err = "Error: function \"" ^ name ^ "\" ends without returning."
             ^ "\n" ^ (format_position pos)
           in fatal_error err
       end
    | stmt -> stmt
  in List.map check_fcn

let scrub stmts =
  let stmts = all_paths_return stmts in
  stmts
