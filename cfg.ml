open Ast
open Types
open Util

type cfg_literal =
  | I32 of int32
  | Bool of bool

type cfg_expr =
  | Expr_FcnCall of string * cfg_expr list
  | Expr_Binary of Ast.operator * cfg_expr * cfg_expr
  | Expr_Unary of Ast.operator * cfg_expr * bool
  | Expr_Literal of cfg_literal
  | Expr_Variable of string
  | Expr_Cast of string * string * cfg_expr

type cfg_basic_block =
  | BB_Cond of conditional_block
  | BB_Expr of Lexing.position * cfg_expr
  | BB_Scope of cfg_scope
  | BB_Return of Lexing.position * cfg_expr
  | BB_ReturnVoid of Lexing.position

and cfg_scope =
  { local_vars : (string * decl) list;
    bbs : cfg_basic_block list
  }

and conditional_block =
  { if_pos       : Lexing.position;
    fi_pos       : Lexing.position;
    cond         : cfg_expr;

    then_scope   : cfg_scope;
    then_returns : bool;

    else_scope   : cfg_scope;
    else_returns : bool
  }

and decl =
  { decl_pos : Lexing.position;
    declname : string;
    tp       : Ast.vartype
  }

type function_defn =
  { defn_begin : Lexing.position;
    defn_end   : Lexing.position;
    name       : string;
    body       : cfg_scope
  }

type program =
  { global_decls : decl Util.symtab;
    fcnlist : function_defn list;
  }

let global_decls decltable = function
  | DefFcn (pos, name, tp, _) ->
     begin match lookup_symbol decltable name with
     | Some _ -> ()
     | None ->
        let fcn =
          { decl_pos = pos;
            declname = name;
            tp = tp }
        in
        add_symbol decltable name fcn
     end
  | _ -> failwith "FIXME: Incomplete implementation Cfg.global_decls."

let get_fcntype_profile = function
  | FcnType (params, ret) -> (params, ret)
  | _ -> fatal_error "Internal error: Unexpected function type."

(** Return true iff the series of Ast.stmts returns on all paths. *)
let rec returns_p stmts =
  let r ret = function
    | StmtExpr _ -> ret
    | Block (_, stmts) -> (returns_p stmts) || ret
    | DefFcn _ -> failwith "FIXME: Unhandled case in Cfg.returns_p"
    | IfStmt (_, _, t, None) -> ret
    | IfStmt (_, _, t, Some e) ->
       ((returns_p t) && (returns_p e)) || ret
    | Return _ -> true
    | ReturnVoid _ -> true
  in List.fold_left r false stmts

let binary_reconcile =
  let types = Hashtbl.create 32 in
  List.iter (fun (n, category, width, _) ->
    Hashtbl.add types n (category, width))
    Types.map_builtin_types;
  let get_type pos name =
    try Hashtbl.find types name
    with _ -> let err = "Error at " ^ (format_position pos) ^ "\n"
                ^ "Invalid type \"" ^ name ^ "\" for the operation:\n"
                ^ (show_source pos) in
              fatal_error err
  in
  let reconcile op (ltype, lexpr) (rtype, rexpr) =
    match op with
    | OperPlus pos
    | OperMinus pos
    | OperMult pos
    | OperDiv pos ->
       let lcategory, lwidth = get_type pos ltype
       and rcategory, rwidth = get_type pos rtype in
       begin match lcategory, rcategory with
       | SignedInteger, SignedInteger ->
          if lwidth == rwidth then ltype, lexpr, rexpr
          else if lwidth < rwidth then
            rtype, Expr_Cast (ltype, rtype, lexpr), rexpr
          else ltype, lexpr, Expr_Cast (rtype, ltype, rexpr)
       end
    | _ -> failwith "FIXME: Incomplete implementation Cfg.reconcile."
  in reconcile

let convert_expr =
  let convert_atom = function
    | AtomInt (pos, i) -> "i32", Expr_Literal (I32 (Int32.of_int i))
    | AtomVar (pos, name) ->
       "i32", Expr_Variable name (* FIXME! Wrong type. *)
  in
  let rec convert = function
    | ExprBinary (op, lhs, rhs) ->
       let tp, lhs, rhs = binary_reconcile op (convert lhs) (convert rhs)
       in tp, Expr_Binary (op, lhs, rhs)
    | ExprAtom atom -> convert_atom atom
    | _ -> failwith "FIXME: Cfg.convert_expr not fully implemented."
  in convert

let cast orig target expr =
(* FIXME: Fake implementation. *)
  if 0 == (String.compare orig target) then expr
  else Expr_Cast (orig, target, expr)

let build_bbs name decltable body =
  let fcndecl = the (lookup_symbol decltable name) in
  let (params, _) = get_fcntype_profile fcndecl.tp in

  (* Add the function's parameters to the scope table. *)
  let fcnscope = push_symtab_scope decltable in
  List.iter
    (fun (pos, name, tp) ->
      add_symbol fcnscope name
        { decl_pos = pos; declname = name; tp = tp })
    params;

  let rec process_bb scope (decls, bbs) = function
    | StmtExpr (pos, expr) ->
       let _, expr = convert_expr expr in
       decls, BB_Expr (pos, expr) :: bbs
    | Block (_, stmts) ->
       let local_decls, local_bbs =
         List.fold_left
           (process_bb (push_symtab_scope scope)) ([], []) stmts
       in
       decls, BB_Scope { local_vars = local_decls;
                         bbs = List.rev local_bbs } :: bbs
    | DefFcn _ ->
       failwith "FIXME: Not implemented Cfg.build_bbs"
    | IfStmt (pos, cond, then_scope, else_block_maybe) ->
       let process_block stmts =
         let local_decls, local_bbs =
           List.fold_left
             (process_bb (push_symtab_scope scope)) ([], []) stmts
         in { local_vars = local_decls; bbs = List.rev local_bbs }
       in
       let else_scope, else_returns = match else_block_maybe with
         | None -> { local_vars = []; bbs = [] }, false
         | Some stmts -> process_block stmts, returns_p stmts
       in
       let tp, conv_cond = convert_expr cond in
       let block =
         { if_pos = pos;
           fi_pos = pos; (* FIXME! *)
           cond   = cast tp "bool" conv_cond;
           then_scope = process_block then_scope;
           then_returns = returns_p then_scope;
           else_scope = else_scope;
           else_returns = else_returns
         }
       in
       decls, (BB_Cond block) :: bbs

    | Return (pos, expr) ->
       let _, expr = convert_expr expr in (* FIXME: Verify return type. *)
       decls, BB_Return (pos, expr) :: bbs
    | ReturnVoid pos ->
       decls, BB_ReturnVoid pos :: bbs
  in
  let decls, bbs = List.fold_left (process_bb fcnscope) ([], []) body in
  { local_vars = decls;
    bbs = List.rev bbs }

let build_fcns decltable fcns = function
  | DefFcn (pos, name, _, body) ->
     let fcn = { defn_begin = pos; defn_end = pos; name = name;
                 body = build_bbs name decltable body }
     in fcn :: fcns
  | _ -> fcns

let convert_ast stmts =
  let decltable = make_symtab () in
  List.iter (global_decls decltable) stmts;
  let fcnlist = List.fold_left (build_fcns decltable) [] stmts
  in
  { global_decls = decltable;
    fcnlist = List.rev fcnlist }
