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
  | IfStmt (pos, _, _, _)
  | Return (pos, _)
  | ReturnVoid pos -> pos

(** Verify that all paths return.  void functions need not have explicit
    returns, but all_paths_return will add them. *)
let all_paths_return =
  let check_body name tp body =
    let retvoid = match tp with
      | VarType (_, "void") -> true
      | _ -> false
    in
    let report_dead_code pos =
      let warn = "Dead code in function \"" ^ name ^ "\": "
        ^ (format_position pos) ^ "\n" ^ (show_source pos)
      in
      warning warn;
      TERMINATED_AND_REPORTED
    in
    let rec check term_p stmt =
      let term_p = match term_p with
        | TERMINATED ->
           report_dead_code (position_of_stmt stmt)
        | _ -> term_p
      in
      match stmt with
      | Return _ | ReturnVoid _ ->
         begin match term_p with
         | NOT_TERMINATED -> TERMINATED
         | _ -> term_p
         end
      | IfStmt (_, _, then_stmts, Some else_stmts) ->
         let then_term = List.fold_left check term_p then_stmts
         and else_term = List.fold_left check term_p else_stmts
         in begin match then_term, else_term with
         | NOT_TERMINATED, _ | _, NOT_TERMINATED -> NOT_TERMINATED
         | _, _ -> TERMINATED_AND_REPORTED
         end
      | Block (pos, stmts) ->
         List.fold_left check term_p stmts
      | _ -> term_p
    in
    match body with
    | StmtExpr (p, e) ->
       if retvoid then StmtExpr (p, e)
       else Return (p, e)
    | Block (pos, stmts) ->
       begin match List.fold_left check NOT_TERMINATED stmts with
       | NOT_TERMINATED ->
          if retvoid then Block (pos, List.append stmts [ReturnVoid pos])
          else raise NoReturn
       | TERMINATED | TERMINATED_AND_REPORTED -> Block (pos, stmts)
       end
    | _ ->
       fatal_error
         "Internal error: all_paths_return: Unexpected function body."
  in
  let check_fcn = function
    | DefFcn (pos, name, tp, body) ->
       begin try DefFcn (pos, name, tp, (check_body name tp body))
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
