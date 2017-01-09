open Cfg

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

let lower_cfg program =
  let no_local_fcns = lift_local_fcns program
  in
  no_local_fcns
