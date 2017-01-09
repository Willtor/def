open Ast
open Lexing
open Util

(* FIXME: This analysis should be done on the CFG instead of the AST.
   Replace this module. *)

exception NoReturn

let faux_pos = { pos_fname = "";
                 pos_lnum = 0;
                 pos_bol = 0;
                 pos_cnum = 0 }

let position_of_stmt = function
  | StmtExpr (pos, _)
  | Block (pos, _)
  | DeclFcn (pos, _, _, _)
  | DefFcn (pos, _, _, _, _)
  | VarDecl ((pos, _, _) :: _, _)
  | IfStmt (pos, _, _, _)
  | WhileLoop (pos, _, _, _)
  | Return (pos, _)
  | ReturnVoid pos
  | TypeDecl (pos, _, _)
  | Label (pos, _)
  | Goto (pos, _)
  | Continue pos
    -> pos
  | VarDecl _ ->
     Report.err_internal __FILE__ __LINE__ "VarDecl with no declarations."

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
      | DeclFcn _ as stmt :: rest -> proc (stmt :: accum) rest
      | DefFcn (pos, vis, name, tp, body) :: rest ->
         let stmt = DefFcn (pos, vis, name, tp, process name body) in
         proc (stmt :: accum) rest
      | VarDecl _ as stmt :: rest ->
         proc (stmt :: accum) rest
      | IfStmt (pos, cond, thenblk, maybe_else) :: rest ->
         let stmt = IfStmt (pos, cond, proc [] thenblk,
                            match maybe_else with
                            | None -> None
                            | Some elseblk -> Some (proc [] elseblk))
         in proc (stmt :: accum) rest
      | WhileLoop (pos, precheck, cond, body) :: rest ->
         let stmt = WhileLoop (pos, precheck, cond, proc [] body)
         in proc (stmt :: accum) rest
      | TypeDecl _ as stmt :: rest ->
         proc (stmt :: accum) rest
      | Label _ as stmt :: rest ->
         proc (stmt :: accum) rest

      | (Return _ as stmt) :: rest
      | (ReturnVoid _ as stmt) :: rest
      | (Goto _ as stmt) :: rest
      | (Continue _ as stmt) :: rest ->
         let () = match rest with
           | [] -> ()
           | extra :: _ -> report_dead_code name (position_of_stmt extra)
         in List.rev (stmt :: accum)
    in proc []
  in
  let toplevel = function
    | DefFcn (pos, vis, name, tp, body) ->
       DefFcn (pos, vis, name, tp, process name body)
    | stmt -> stmt
  in List.map toplevel

(** Expression cannot resolve to false/0.  *)
let expr_must_be_true = function
  | ExprLit (_, LitBool true) -> true
  | _ -> false

let return_all_paths =
  let process can_return_void body =
    let rec contains_return = function
      | [] -> false
      | Return _ :: _
      | ReturnVoid _ :: _ -> true
      | Block (_, body) :: rest ->
         if contains_return body then true
         else contains_return rest
      | IfStmt (_, _, tstmts, None) :: rest ->
         if contains_return tstmts then true
         else contains_return rest
      | IfStmt (_, _, tstmts, Some estmts) :: rest ->
         if contains_return tstmts then true
         else if contains_return estmts then true
         else contains_return rest
      | WhileLoop (_, _, _, body) :: rest ->
         if contains_return body then true
         else contains_return rest
      | DefFcn _ :: rest (* Returns from nested functions don't count. *)
      | _ :: rest -> contains_return rest
    in
    let rec returns_p = function
      | [] -> false
      | Return _ :: _
      | ReturnVoid _ :: _ -> true
      | Block (_, block) :: rest ->
         if returns_p block then true
         else returns_p rest
      | StmtExpr _ :: rest
      | DeclFcn _ :: rest
      | DefFcn _ :: rest
      | VarDecl _ :: rest
      | IfStmt (_, _, _, None) :: rest
      | TypeDecl _ :: rest
      | Label _ :: rest
      | Goto _ :: rest (* FIXME: Think about Goto case some more... *)
      | Continue _ :: rest (* FIXME: Also, the Continue case. *)
        -> returns_p rest
      | IfStmt (_, _, then_branch, Some else_branch) :: rest ->
         if (returns_p then_branch) && (returns_p else_branch) then true
         else returns_p rest
      | WhileLoop (_, _, cond, body) :: rest ->
         if expr_must_be_true cond then contains_return body
         else returns_p rest
    in
    if returns_p body then body
    else if can_return_void then
      (* Implicit return statement.  This is only allowed when the return
         type is void. *)
      List.append body [ ReturnVoid faux_pos ]
    else raise NoReturn
  in
  let toplevel = function
    | DefFcn (pos, vis, name, tp, body) ->
       let can_return_void = match tp with
         | FcnType (_, VarType (_, "void")) -> true
         | _ -> false
       in
       begin try DefFcn (pos, vis, name, tp, process can_return_void body)
         with _ -> (* Fixme: Need the end of function position. *)
           Report.err_no_return pos name
       end
    | stmt -> stmt
  in List.map toplevel

let scrub stmts =
  let stmts = kill_dead_code stmts in
  let stmts = return_all_paths stmts in
  stmts
