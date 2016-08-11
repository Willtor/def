open Lexing
open Ast

type prototype =
  { name   : string; (* Do we need this field? *)
    ret    : vartype;
    params : (position * string * vartype) list;
    decl   : position
  }

let fcn_set = Hashtbl.create 128

let fcn_find name =
  try Some (Hashtbl.find fcn_set name)
  with Not_found -> None

let fcn_add = Hashtbl.add fcn_set

let verify_expr e = e

let verify_fcn_decl = function
  | NamedFunction (decl, name, params, ret) as fcn ->
     begin match fcn_find name with
     | None ->
        begin
          fcn_add name
            { name = name;
              ret = ret;
              params = params;
              decl = decl };
          fcn
        end
     | Some _ ->
        failwith ("Function " ^ name ^ " was already defined.")
     end

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
            let verified_fn = verify_fcn_decl fn in
            let verified_stmt = verify_stmt stmt in
            DefFcn (verified_fn, verified_stmt)
         | Return e ->
            let verified_e = verify_expr e in Return verified_e
       in
       verify (verify_stmt stmt :: accum) rest
    | [] -> List.rev accum
  in verify []

