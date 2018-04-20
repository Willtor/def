(* Copyright (C) 2017  DEFC Authors

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License
   as published by the Free Software Foundation; either version 2
   of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
   02110-1301, USA.
 *)

open Ast
open Error
open Lexing
open Parsetree
open Types
open Util

(* FIXME: This analysis should be done on the CFG instead of the AST.
   Replace this module. *)

exception NoReturn

let position_of_stmt = function
  | StmtExpr (pos, _)
  | Block (pos, _)
  | DeclFcn (pos, _, _, _, _)
  | DefFcn (pos, _, _, _, _, _, _)
  | DefTemplateFcn (pos, _, _, _, _, _, _)
  | VarDecl ({td_pos = pos}, _, _, _, _)
  | InlineStructVarDecl ({td_pos = pos}, _, _)
  | TransactionBlock (pos, _)
  | IfStmt (pos, _, _, _)
  | ForLoop (pos, _, _, _, _, _, _)
  | WhileLoop (pos, _, _, _)
  | SwitchStmt (pos, _, _)
  | Return (pos, _)
  | ReturnVoid pos
  | TypeDecl (pos, _, _, _, _)
  | Label (pos, _)
  | Goto (pos, _)
  | Break pos
  | Continue pos
  | Sync pos
    -> pos
  | Import (tok, _) -> tok.td_pos

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
      | DefFcn (pos, doc, vis, name, tp, params, body) :: rest ->
         let stmt = DefFcn (pos, doc, vis, name, tp, params, process name body)
         in proc (stmt :: accum) rest
      | DefTemplateFcn _ :: _ ->
         Report.err_internal __FILE__ __LINE__ "unexpanded template."
      | VarDecl _ as stmt :: rest ->
         proc (stmt :: accum) rest
      | InlineStructVarDecl _ as stmt :: rest ->
         proc (stmt :: accum) rest
      | TransactionBlock (pos, body) :: rest ->
         let stmt = TransactionBlock (pos, proc [] body) in
         proc (stmt :: accum) rest
      | IfStmt (pos, cond, thenblk, maybe_else) :: rest ->
         let stmt = IfStmt (pos, cond, proc [] thenblk,
                            match maybe_else with
                            | None -> None
                            | Some (epos, elseblk) ->
                               Some (epos, (proc [] elseblk)))
         in proc (stmt :: accum) rest
      | ForLoop (pos, is_parallel, init, cond, iter, dpos, body) :: rest ->
         let stmt = ForLoop (pos, is_parallel, init, cond, iter, dpos,
                             proc [] body)
         in proc (stmt :: accum) rest
      | WhileLoop (pos, precheck, cond, body) :: rest ->
         let stmt = WhileLoop (pos, precheck, cond, proc [] body)
         in proc (stmt :: accum) rest
      | SwitchStmt (pos, expr, cases) :: rest ->
         let f (cpos, fall, ctor, stmts) = cpos, fall, ctor, proc [] stmts in
         let stmt = SwitchStmt (pos, expr, List.map f cases) in
         proc (stmt :: accum) rest
      | TypeDecl _ as stmt :: rest ->
         proc (stmt :: accum) rest
      | Label _ as stmt :: rest ->
         proc (stmt :: accum) rest
      | Sync _ as stmt :: rest ->
         proc (stmt :: accum) rest
      | (Return _ as stmt) :: rest
      | (ReturnVoid _ as stmt) :: rest
      | (Goto _ as stmt) :: rest
      | (Break _ as stmt) :: rest
      | (Continue _ as stmt) :: rest ->
         let () = match rest with
           | [] -> ()
           | extra :: _ -> report_dead_code name (position_of_stmt extra)
         in List.rev (stmt :: accum)
      | Import _ :: _ ->
         Report.err_internal __FILE__ __LINE__ "import in function."
    in proc []
  in
  let toplevel = function
    | DefFcn (pos, doc, vis, name, tp, params, body) ->
       DefFcn (pos, doc, vis, name, tp, params, process name body)
    | stmt -> stmt
  in List.map toplevel

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
      | IfStmt (_, _, tstmts, Some (_, estmts)) :: rest ->
         if contains_return tstmts then true
         else if contains_return estmts then true
         else contains_return rest
      | ForLoop (_, _, _, _, _, _, body) :: rest
      | WhileLoop (_, _, _, body) :: rest ->
         if contains_return body then true
         else contains_return rest
      | SwitchStmt (_, _, cases) :: rest ->
         let do_case res (_, _, _, stmts) =
           if true = res then true
           else contains_return stmts
         in
         if List.fold_left do_case false cases then true
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
      | InlineStructVarDecl _ :: rest
      | TransactionBlock _ :: rest
      | IfStmt (_, _, _, None) :: rest
      | TypeDecl _ :: rest
      | Label _ :: rest
      | Goto _ :: rest (* FIXME: Think about Goto case some more... *)
      | Break _ :: rest (* FIXME: Also the break case. *)
      | Continue _ :: rest (* FIXME: Also, the Continue case. *)
      | Sync _ :: rest
        -> returns_p rest
      | IfStmt (_, _, then_branch, Some (_, else_branch)) :: rest ->
         if (returns_p then_branch) && (returns_p else_branch) then true
         else returns_p rest
      | ForLoop _ :: rest ->
         returns_p rest
      | WhileLoop (_, _, cond, body) :: rest ->
         if provably_always_true cond then contains_return body
         else returns_p rest
      | SwitchStmt _ :: rest ->
         returns_p rest
      | Import _ :: _ ->
         Report.err_internal __FILE__ __LINE__ "import in function."
      | DefTemplateFcn _ :: _ ->
         Report.err_internal __FILE__ __LINE__ "unexpanded template."
    in
    if returns_p body then body
    else if can_return_void then
      (* Implicit return statement.  This is only allowed when the return
         type is void. *)
      List.append body [ ReturnVoid faux_pos ]
    else raise NoReturn
  in
  let toplevel = function
    | DefFcn (pos, doc, vis, name, tp, params, body) ->
       let can_return_void = match tp.bare with
         | DefTypeFcn (_, { bare = DefTypeVoid }, _) -> true
         | _ -> false
       in
       begin
         try DefFcn (pos, doc, vis, name, tp, params,
                     process can_return_void body)
         with _ -> (* Fixme: Need the end of function position. *)
           Report.err_no_return pos name
       end
    | stmt -> stmt
  in List.map toplevel

let scrub stmts =
  return_all_paths
  @@ kill_dead_code stmts
