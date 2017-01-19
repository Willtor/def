open Ast
open Cfg
open Types

let lift_local_fcns program =
  let rec search fcnlist fcn =
    let rec iter locals accum = function
      | [] -> locals, List.rev accum
      | BB_Cond (label, cblock) :: rest ->
         let then_locals, then_bbs = iter locals [] cblock.then_scope in
         let else_locals, else_bbs = iter then_locals [] cblock.else_scope in
         let () = cblock.then_scope <- then_bbs
         and () = cblock.else_scope <- else_bbs in
         iter else_locals (BB_Cond (label, cblock) :: accum) rest
      | BB_Loop (label, loop) :: rest ->
         let body_locals, body_bbs = iter locals [] loop.body_scope in
         let () = loop.body_scope <- body_bbs in
         iter body_locals (BB_Loop (label, loop) :: accum) rest
      | BB_LocalFcn f :: rest ->
         let nested_locals = search locals f in
         let subsequent_locals, bbs = iter nested_locals accum rest in
         f :: subsequent_locals, bbs
      | bb :: rest ->
         iter locals (bb :: accum) rest
    in
    let locals, bbs = iter fcnlist [] fcn.bbs in
    let () = fcn.bbs <- bbs in
    locals
  in
  let fcnlist = List.fold_left search program.fcnlist program.fcnlist
  in
  { global_decls = program.global_decls;
    fcnlist = fcnlist;
    deftypemap = program.deftypemap
  }

let lift_lhs_static_structs program =
  let lift fcn =
    (* FIXME: This is a special case fix.  Very bad.  But it will have to be
       rewritten, anyway, when the cfg is redone. *)
    let rec repair_expr vaccum elist = function
      | Expr_Binary (OperAssign,
                     DefTypeLiteralStruct (mtypes, mnames),
                     Expr_StaticStruct members,
                     rhs) ->
         let () = prerr_endline "Rewriting static struct assignment." in
         let decl = { decl_pos = fcn.defn_begin;
                      mappedname = "__defstatic";
                      vis = VisLocal;
                      tp = DefTypeLiteralStruct (mtypes, mnames);
                      params = []
                    } in
         let vaccum = ("__defstatic", decl) :: vaccum in
         let exprs = List.mapi (fun n (tp, e) ->
           Expr_Binary (OperAssign, tp, e,
                        Expr_SelectField (Expr_Variable "__defstatic", n)))
           members
         in
         vaccum,
         Expr_Binary (OperAssign,
                      DefTypeLiteralStruct (mtypes, mnames),
                      Expr_Variable "__defstatic", rhs) :: exprs
      | e -> vaccum, elist
    in
    let rec visit vaccum baccum = function
      | [] -> vaccum, List.rev baccum
      | BB_Expr (p, l, expr) :: rest ->
         let vaccum, elist = repair_expr vaccum [] expr in
         let bbelist = List.map (fun e -> BB_Expr (p, l, e)) elist in
         visit vaccum (bbelist @ baccum) rest
      | BB_Cond (l, cblock) :: rest ->
         let vaccum, ts = visit vaccum [] cblock.then_scope in
         let vaccum, es = visit vaccum [] cblock.else_scope in
         begin
           cblock.then_scope <- ts;
           cblock.else_scope <- es;
           visit vaccum (BB_Cond (l, cblock) :: baccum) rest
         end
      | BB_Loop (l, lblock) :: rest ->
         let vaccum, s = visit vaccum [] lblock.body_scope in
         begin
           lblock.body_scope <- s;
           visit vaccum (BB_Loop (l, lblock) :: baccum) rest
         end
      | bb :: rest ->
         visit vaccum (bb :: baccum) rest
    in
    let vars, bbs = visit [] [] fcn.bbs in
    { defn_begin = fcn.defn_begin;
      defn_end = fcn.defn_end;
      name = fcn.name;
      local_vars = vars @ fcn.local_vars;
      bbs = bbs
    }
  in
  { global_decls = program.global_decls;
    fcnlist = List.map lift program.fcnlist;
    deftypemap = program.deftypemap
  }

let lower_cfg program =
  let program = lift_local_fcns program in
  let program = lift_lhs_static_structs program in
  program
