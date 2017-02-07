open Ast
open Cfg
open Types

(*
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
*)

let lift_lhs_static_structs program =
  let lift fcn =
    (* FIXME: Naming needs to be fixed so there are no conflicts. *)
    (* FIXME: Special-case.  Need something more general/correct. *)
    let rec repair_expr (pos, expr) =
      match expr with
      | Expr_Binary (OperAssign,
                     DefTypeStaticStruct mtypes,
                     Expr_StaticStruct members,
                     rhs) ->
         let decl = { decl_pos = fcn.defn_begin;
                       mappedname = "__defstatic";
                       vis = VisLocal;
                       tp = DefTypeStaticStruct mtypes;
                       params = []
                     } in
         let vars = ("__defstatic", decl) in
         let exprs = List.mapi (fun n (tp, e) ->
           pos,
           Expr_Binary (OperAssign, tp, e,
                        Expr_SelectField (Expr_Variable "__defstatic", n)))
           members
         in
         [ vars ],
         (pos, Expr_Binary (OperAssign,
                            DefTypeStaticStruct mtypes,
                            Expr_Variable "__defstatic", rhs)) :: exprs
      | Expr_Binary (OperAssign,
                     DefTypeLiteralStruct (mtypes, mnames),
                     Expr_StaticStruct members,
                     rhs) ->
         let decl = { decl_pos = fcn.defn_begin;
                      mappedname = "__defstatic";
                      vis = VisLocal;
                      tp = DefTypeLiteralStruct (mtypes, mnames);
                      params = []
                    } in
         let vars = ("__defstatic", decl) in
         let exprs = List.mapi (fun n (tp, e) ->
           pos,
           Expr_Binary (OperAssign, tp, e,
                        Expr_SelectField (Expr_Variable "__defstatic", n)))
           members
         in
         [ vars ],
         (pos, Expr_Binary (OperAssign,
                            DefTypeLiteralStruct (mtypes, mnames),
                            Expr_Variable "__defstatic", rhs)) :: exprs
      | _ -> [], [ (pos, expr) ]
    in
    let visit vars = function
      | BB_Seq (_, block) ->
         let new_vars, elist =
           List.split (List.map repair_expr block.seq_expr) in
         let () = block.seq_expr <- (List.concat elist) in
         List.concat (vars :: new_vars)
      | BB_Cond _ -> vars
      | BB_Term _ -> vars (* FIXME: Need to fix this up, ever? *)
      | _ -> Report.err_internal __FILE__ __LINE__
         "Unexpected basic block type."
    in
    let vars = Cfg.visit_df visit true fcn.local_vars fcn.entry_bb in
    { defn_begin = fcn.defn_begin;
      defn_end = fcn.defn_end;
      name = fcn.name;
      local_vars = vars;
      entry_bb = fcn.entry_bb
    }
  in
  { global_decls = program.global_decls;
    fcnlist = List.map lift program.fcnlist;
    deftypemap = program.deftypemap
  }

let lower_cfg program =
  (* let program = lift_local_fcns program in *)
  let program = lift_lhs_static_structs program in
  program
