(* Copyright (C) 2018  DEFC Authors

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
open Llvm
open Llvmext (* build_cmpxchg, token_type *)
open Llvm_target
open Operator
open Parsetree
open Scrubber
open Str
open Types
open Util

type data =
  { ctx         : llcontext;
    mdl         : llmodule;
    bldr        : llbuilder;
    prog        : program;

    mutable difile  : llvalue option;
    mutable dib     : lldibuilder option;

    globalsyms  : stmt symtab;
    globalvals  : llvalue symtab;
    lltypesyms  : lltype symtab;

    mutable curr_bb : llbasicblock option;
  }

let va_list_tag = "struct.__va_list_tag"
let frameaddress = "llvm.frameaddress"
let stacksave = "llvm.stacksave"
let setjmp = "llvm.eh.sjlj.setjmp"
let longjmp = "llvm.eh.sjlj.longjmp"
let syncregion_start = "llvm.syncregion.start"

let maybe_apply f = function
  | None -> ()
  | Some x -> f x

let const_stringz_wrap =
  let re_set =
    [ (regexp {|\\n|}, "\n");    (* newline *)
      (regexp {|\\r|}, "\r");    (* carriage return *)
      (regexp {|\\t|}, "\t");    (* tab *)
      (regexp {|\\"|}, "\"");    (* double-quote. *)
      (regexp {|\\\\|}, "\\\\")  (* backslash: must be last. *)
    ]
  in
  fun ctx str ->
  let replaced =
    List.fold_left
      (fun s (re, subst) -> global_replace re subst s)
      str
      re_set
  in
  const_stringz ctx replaced

let basic_lltypes ctx =
  let typesyms = make_symtab () in
  let add_type (name, _, ctor, _, _, _) =
    add_symbol typesyms name (ctor ctx)
  in
  List.iter add_type map_builtin_types;
  typesyms

let make_module_flags data =
  if !Config.position_indep then
    let pic_level =
      mdnode data.ctx
             [| const_int (i32_type data.ctx) 1;
                mdstring data.ctx "PIC Level";
                const_int (i32_type data.ctx) 2 |]
    in
    add_named_metadata_operand data.mdl "llvm.module.flags" pic_level

let debug_sym_preamble data module_name =
  let i32 = const_int (i32_type data.ctx) in
  let version_string =
    (string_of_int Version.version_maj)
    ^ "." ^ (string_of_int Version.version_min)
    ^ "." ^ (string_of_int Version.version_patch)
    ^ Version.version_suffix
  in
  let defident = "def version " ^ version_string in
  let dib = dibuilder data.mdl in
  (* FIXME: basename should be expanded so it isn't "." or whatever. *)
  let file = difile data.ctx dib (Filename.basename module_name)
                    (Filename.dirname module_name)
  in
  let _ = dicompile_unit data.ctx dib file defident
                         (!Config.opt_level <> 0) "" 0 in
  data.difile <- Some file;
  data.dib <- Some dib;
  let dwarf_version =
    mdnode data.ctx [| i32 2; mdstring data.ctx "Dwarf Version"; i32 4 |]
  in
  let debug_info_version =
    mdnode data.ctx [| i32 2; mdstring data.ctx "Debug Info Version"; i32 3 |]
  in
  add_named_metadata_operand data.mdl "llvm.module.flags" dwarf_version;
  add_named_metadata_operand data.mdl "llvm.module.flags" debug_info_version;
  add_named_metadata_operand data.mdl "llvm.ident" (mdstring data.ctx defident)

(** Convert a deftype to an equivalent lltype.  Note: All function types are
    treated as function pointers.  When declaring a function, the deref'd type
    must be retrieved. *)
let get_or_make_type data =
  let native str = Util.the (lookup_symbol data.lltypesyms str) in
  let rec get_or_make deftype =
    match deftype.bare with
    | DefTypeUnresolved name ->
       Report.err_internal __FILE__ __LINE__ ("unresolved " ^ name)
    | DefTypeVoid -> native "void"
    | DefTypeNamed name ->
       begin
         match lookup_symbol data.lltypesyms name with
         | Some v -> v
         | None ->
            begin
              match Util.the (lookup_symbol data.prog.prog_typemap name) with
              | { bare = DefTypeLiteralStruct (is_packed, mtypes, _) } ->
                 let structtype = named_struct_type data.ctx name in
                 let () = add_symbol data.lltypesyms name structtype in
                 let members = List.map get_or_make mtypes in
                 let () = struct_set_body structtype
                            (Array.of_list members) is_packed in
                 structtype
              | subtype ->
                 let lltype = get_or_make subtype in
                 let () = add_symbol data.lltypesyms name lltype in
                 lltype
            end
       end
    | DefTypeOpaque nm ->
       Report.err_internal __FILE__ __LINE__ ("opaque type " ^ nm)
    | DefTypePrimitive prim ->
       native (primitive2string prim)
    | DefTypeFcn (params, ret, is_vararg) ->
       let llvmparams = List.map get_or_make params in
       let llvmret = get_or_make ret in
       let f = if is_vararg then var_arg_function_type else function_type in
       let llfcntype = f llvmret (Array.of_list llvmparams) in
       pointer_type llfcntype
    | DefTypePtr ({ bare = DefTypeVoid })
    | DefTypePtr ({ bare = DefTypeOpaque _ }) ->
       pointer_type (native "i8")
    | DefTypePtr subtype ->
       pointer_type (get_or_make subtype)
    | DefTypeArray (subtype, dim) ->
       array_type (get_or_make subtype) dim
    | DefTypeNullPtr ->
       Report.err_internal __FILE__ __LINE__ "null pointer type"
    | DefTypeEnum _ ->
       let sz = size_of data.prog.prog_typemap deftype in
       integer_type data.ctx (sz * 8)
    | DefTypeLiteralStruct (is_packed, mtypes, _)
    | DefTypeStaticStruct (is_packed, mtypes) ->
       let members = List.map get_or_make mtypes in
       let f = if is_packed then packed_struct_type else struct_type in
       f data.ctx (Array.of_list members)
    | DefTypeLiteralUnion _ ->
       (* FIXME: This should take alignment into account.  There're going to
          be troubles if we don't. *)
       let sz = Types.size_of data.prog.prog_typemap deftype in
       let array = array_type (native "i8") sz in
       struct_type data.ctx [| array |]
    | DefTypeVAList ->
       native va_list_tag
    | DefTypeWildcard ->
       Report.err_internal __FILE__ __LINE__ "wildcard type."
    | DefTypeLLVMToken ->
       token_type data.ctx
  in
  get_or_make

let build_cast data raw_from raw_to e =
  let from_tp = concrete_of None data.prog.prog_typemap raw_from
  and to_tp = concrete_of None data.prog.prog_typemap raw_to
  in

  let build_primitive_cast t1 t2 =
    let llvm_to_tp =
      Util.the (lookup_symbol data.lltypesyms (primitive2string t2)) in
    let null_cast _ _ _ _ = e in
    let f = match t1, t2 with

      (* From bool *)
      | PrimBool, PrimI8  | PrimBool, PrimU8
      | PrimBool, PrimI16 | PrimBool, PrimU16
      | PrimBool, PrimI32 | PrimBool, PrimU32
      | PrimBool, PrimI64 | PrimBool, PrimU64 -> build_zext
      | PrimBool, PrimF32
      | PrimBool, PrimF64 -> build_uitofp

      (* From i8 *)
      | PrimI8, PrimBool -> build_trunc
      | PrimI8, PrimU8 -> null_cast
      | PrimI8, PrimI16 | PrimI8, PrimI32 | PrimI8, PrimI64 -> build_sext
      | PrimI8, PrimU16 | PrimI8, PrimU32 | PrimI8, PrimU64 -> build_zext
      | PrimI8, PrimF32
      | PrimI8, PrimF64 -> build_sitofp

      (* From u8 *)
      | PrimU8, PrimBool -> build_trunc
      | PrimU8, PrimI8 -> null_cast
      | PrimU8, PrimI16 | PrimU8, PrimI32 | PrimU8, PrimI64
      | PrimU8, PrimU16 | PrimU8, PrimU32 | PrimU8, PrimU64 -> build_zext
      | PrimU8, PrimF32
      | PrimU8, PrimF64 -> build_sitofp

      (* From i16 *)
      | PrimI16, PrimBool
      | PrimI16, PrimU8  | PrimI16, PrimI8 -> build_trunc
      | PrimI16, PrimU16 -> null_cast
      | PrimI16, PrimI32 | PrimI16, PrimI64 -> build_sext
      | PrimI16, PrimU32 | PrimI16, PrimU64 -> build_zext
      | PrimI16, PrimF32
      | PrimI16, PrimF64 -> build_sitofp

      (* From u16 *)
      | PrimU16, PrimBool
      | PrimU16, PrimU8  | PrimU16, PrimI8 -> build_trunc
      | PrimU16, PrimI16 -> null_cast
      | PrimU16, PrimI32 | PrimU16, PrimU32
      | PrimU16, PrimI64 | PrimU16, PrimU64 -> build_zext
      | PrimU16, PrimF32
      | PrimU16, PrimF64 -> build_uitofp

      (* From i32 *)
      | PrimI32, PrimBool
      | PrimI32, PrimU8  | PrimI32, PrimI8
      | PrimI32, PrimI16 | PrimI32, PrimU16 -> build_trunc
      | PrimI32, PrimU32 -> null_cast
      | PrimI32, PrimI64 -> build_sext
      | PrimI32, PrimU64 -> build_zext
      | PrimI32, PrimF32
      | PrimI32, PrimF64 -> build_sitofp

      (* From u32 *)
      | PrimU32, PrimBool
      | PrimU32, PrimU8  | PrimU32, PrimI8
      | PrimU32, PrimI16 | PrimU32, PrimU16 -> build_trunc
      | PrimU32, PrimI32 -> null_cast
      | PrimU32, PrimI64 | PrimU32, PrimU64 -> build_zext
      | PrimU32, PrimF32
      | PrimU32, PrimF64 -> build_uitofp

      (* From i64 *)
      | PrimI64, PrimBool
      | PrimI64, PrimU8  | PrimI64, PrimI8
      | PrimI64, PrimI16 | PrimI64, PrimU16
      | PrimI64, PrimI32 | PrimI64, PrimU32 -> build_trunc
      | PrimI64, PrimU64 -> null_cast
      | PrimI64, PrimF32
      | PrimI64, PrimF64 -> build_sitofp

      (* From u64 *)
      | PrimU64, PrimBool
      | PrimU64, PrimU8  | PrimU64, PrimI8
      | PrimU64, PrimI16 | PrimU64, PrimU16
      | PrimU64, PrimI32 | PrimU64, PrimU32 -> build_trunc
      | PrimU64, PrimI64 -> null_cast
      | PrimU64, PrimF32
      | PrimU64, PrimF64 -> build_uitofp

      (* From f32 *)
      | PrimF32, PrimBool
      | PrimF32, PrimI8 | PrimF32, PrimI16 | PrimF32, PrimI32
      | PrimF32, PrimI64 -> build_fptosi
      | PrimF32, PrimU8 | PrimF32, PrimU16 | PrimF32, PrimU32
      | PrimF32, PrimU64 -> build_fptoui
      | PrimF32, PrimF64 -> build_fpext

      (* From f64 *)
      | PrimF64, PrimBool
      | PrimF64, PrimI8 | PrimF64, PrimI16 | PrimF64, PrimI32
      | PrimF64, PrimI64 -> build_fptosi
      | PrimF64, PrimU8 | PrimF64, PrimU16 | PrimF64, PrimU32
      | PrimF64, PrimU64 -> build_fptoui
      | PrimF64, PrimF32 -> build_fptrunc

      | _ -> Report.err_internal __FILE__ __LINE__
               ("Cast from " ^ (primitive2string t1) ^ " to "
                ^ (primitive2string t2))
    in
    f e llvm_to_tp "cast" data.bldr
  in

  match from_tp.bare, to_tp.bare with
  | DefTypePrimitive prim1, DefTypePrimitive prim2 ->
     if prim1 = prim2 then e
     else build_primitive_cast prim1 prim2
  | DefTypePtr _, DefTypePtr _ ->
     let lltype = get_or_make_type data to_tp in
     build_bitcast e lltype "cast" data.bldr
  | DefTypePtr _, DefTypePrimitive _ ->
     if is_integer_type to_tp then
       let lltype = get_or_make_type data to_tp in
       build_pointercast e lltype "cast" data.bldr
     else Report.err_internal __FILE__ __LINE__
            "What's the deal with casting pointers to floats?"
  | DefTypePrimitive _, DefTypePtr _ ->
     if is_integer_type from_tp then
       let lltype = get_or_make_type data to_tp in
       build_inttoptr e lltype "cast" data.bldr
     else Report.err_internal __FILE__ __LINE__
            "What's the deal with casting floats to ptrs?"
  | DefTypeStaticStruct _, DefTypeNamed _
  | DefTypeLiteralStruct _, DefTypeNamed _ ->
     (* FIXME: This can never happen, right? *)
     let () = prerr_endline "unexpected case in casting." in
     let lltype = get_or_make_type data to_tp in
     build_bitcast e lltype "scast" data.bldr
  | DefTypeStaticStruct _, _
  | DefTypeLiteralStruct _, _ ->
     (* FIXME: I think this might be correct, actually, but need to think
        about it some more. *)
     e
  | DefTypeArray (tp, n), DefTypePtr _ ->
     let zero = const_null (i32_type data.ctx) in
     build_in_bounds_gep e [| zero; zero |] "ptr_cast" data.bldr
  | DefTypeFcn _, DefTypeFcn _ ->
     let lltype = get_or_make_type data (makeptr to_tp) in
     build_pointercast e lltype "fcast" data.bldr
  | DefTypeEnum _, DefTypeEnum _ ->
     e
  | _ ->
     Report.err_internal __FILE__ __LINE__
       ("build_cast: Incomplete implementation (from "
        ^ (string_of_type from_tp) ^ " to "
        ^ (string_of_type to_tp) ^ ").")

let const_literal typesyms lit =
  let native str = Util.the @@ lookup_symbol typesyms str in
  match lit with
  | LitBool true -> const_int (native "bool") 1
  | LitBool false -> const_int (native "bool") 0
  | LitI8 n
  | LitU8 n -> const_int (native "i8") (Char.code n)
  | LitI16 n
  | LitU16 n
  | LitI32 n
  | LitU32 n ->
     let typename = primitive2string (literal2primitive lit) in
     const_int (native typename) (Int32.to_int n)
  | LitI64 n -> const_of_int64 (native "i64") n true
  | LitU64 n -> const_of_int64 (native "i64") n false
  | LitF32 n -> const_float (native "f32") n
  | LitF64 n -> const_float (native "f64") n

let rec get_const_val data expr =
  match expr.expr_ast with
  | ExprString str -> const_stringz_wrap data.ctx str
  | ExprLit lit -> const_literal data.lltypesyms lit
  | ExprPreUnary ({ op_op = OperMinus; op_left = left }) ->
     const_neg @@ get_const_val data left
  | _ ->
     Report.err_internal __FILE__ __LINE__
       "non-implemented case of get_const_val"

let get_or_make_val data scope name =
  (* Any local value has already been declared.  If it isn't in the local map,
     then it's a global value.  If it isn't in the global map, then we simply
     haven't declared it, yet. *)
  match lookup_symbol scope name with
  | Some v -> v
  | None ->
     match lookup_symbol data.globalvals name with
     | Some v -> v
     | None ->
        (* It's a global value and it hasn't been declared to LLVM, yet. *)
        match Util.the (lookup_symbol data.globalsyms name) with
        | VarDecl (decl, names, inits, deftype, vis) ->
           let lltype = get_or_make_type data deftype in
           let v = declare_global lltype name data.mdl in
           let () = if decl.td_text = "var" then set_thread_local true v in
           let () = add_symbol data.globalvals name v in
           let () =
             if inits <> [] then
               List.iter2
                 (fun n i ->
                   if n.td_text = name then
                     match i.expr_ast with
                     | ExprBinary op ->
                        let rhs = Util.the op.op_right in
                        let init_val = get_const_val data rhs in
                        set_initializer init_val v
                     | _ ->
                        Report.err_internal __FILE__ __LINE__
                          "unexpected global initializer")
                 names
                 inits
             else if vis <> Types.VisExternal then
               let init_val =
                 match deftype.bare with
                 | DefTypePrimitive _ ->
                    const_null (get_or_make_type data deftype)
                 | _ ->
                    Report.err_internal __FILE__ __LINE__
                      ("no default initializer for "
                       ^ (string_of_type deftype))
               in
               set_initializer init_val v
           in
           v
        | DeclFcn (_, vis, _, deftype, _)
        | DefFcn (_, _, vis, _, deftype, _, _) ->
           (* get_or_make_type will give us a pointer-to-a-function type.  So
              we get the element_type, since this is the function, itself. *)
           let lltype = element_type @@ get_or_make_type data deftype in
           let llfcn = declare_function name lltype data.mdl in
           let () = if vis = VisLocal then
                      set_linkage Linkage.Internal llfcn
           in
           let () = add_symbol data.globalvals name llfcn in
           llfcn
        | _ ->
           Report.err_internal __FILE__ __LINE__ "unexpected symbol type."

let gather_global data stmt =
  match stmt with
  | VarDecl (_, names, _, _, _) ->
     List.iter
       (fun tok -> add_symbol data.globalsyms tok.td_text stmt)
       names
  | DeclFcn (_, _, name, _, _)
  | DefFcn (_, _, _, name, _, _, _) ->
     add_symbol data.globalsyms name stmt
  | _ -> ()

let ir_gen data llfcn fcn_scope entry fcn_body =

  let set_curr_bb bb =
    data.curr_bb <- Some bb;
    position_at_end bb data.bldr
  in

  let break_bb = ref None in

  let save_break_bb () =
    let saved = !break_bb in
    break_bb := None;
    saved
  in

  let continue_bb = ref [] in

  let push_continue_bb bb = continue_bb := bb :: !continue_bb in
  let pop_continue_bb () = continue_bb := List.tl !continue_bb in

  let labeled_bbs = make_symtab () in

  let get_or_make_bb label =
    match lookup_symbol labeled_bbs label with
    | Some bb -> bb
    | None ->
       let bb = append_block data.ctx label llfcn in
       let () = add_symbol labeled_bbs label bb in
       bb
  in

  let bb_needs_exit bb =
    match instr_end bb with
    | At_start _ -> true
    | After instr ->
       match classify_value instr with
       | ValueKind.Instruction Opcode.Ret
       | ValueKind.Instruction Opcode.Br
       | ValueKind.Instruction Opcode.IndirectBr -> false
       | _ -> true
  in

  let curr_sync_region = ref None in

  let get_or_make_sync_region scope =
    match !curr_sync_region with
    | Some region -> region
    | None ->
       begin
         let restore_bb = Util.the data.curr_bb in
         set_curr_bb entry;
         let label = "sync_region." ^ (Util.unique_id ()) in
         let sr_start = get_or_make_val data scope syncregion_start in
         let sr = build_call sr_start [| |] label data.bldr in
         curr_sync_region := Some sr;
         set_curr_bb restore_bb;
         sr
       end
  in

  let select_for_type sfcn ufcn ffcn tp =
    if is_integer_type tp then
      if signed_p tp then sfcn
      else ufcn
    else if is_pointer_type tp then ufcn
    else ffcn
  in

  let build_load_wrapper deftype lladdr name =
    let llval = build_load lladdr name data.bldr in
    if deftype.dtvolatile then set_volatile true llval;
    llval
  in

  let rec declare_var scope lltype init_maybe vname =
    position_at_end entry data.bldr;
    let alloc = build_alloca lltype vname data.bldr in
    add_symbol scope vname alloc;
    position_at_end (Util.the data.curr_bb) data.bldr;
    if init_maybe <> None then
      ignore(expr_gen scope true (Util.the init_maybe));
    alloc

  and builtin_gen scope name args =

    (* FIXME: This matching thing is bad.  Unify this code with Scrubber. *)

    match name with
    | "__builtin_swap" ->
       let dest = expr_gen scope true (List.nth args 0) in
       let v = expr_gen scope true (List.nth args 1) in
       build_atomicrmw AtomicRMWBinOp.Xchg dest v
         AtomicOrdering.SequentiallyConsistent
         false
         ""
         data.bldr

    | "__builtin_cas" ->
       let dest = expr_gen scope true (List.nth args 0) in
       let cmp = expr_gen scope true (List.nth args 1) in
       let v = expr_gen scope true (List.nth args 2) in
       let cmpxchg = build_cmpxchg dest cmp v
                       AtomicOrdering.SequentiallyConsistent
                       AtomicOrdering.SequentiallyConsistent
                       ""
                       data.bldr
       in
       build_extractvalue cmpxchg 1 "" data.bldr

    | "__builtin_setjmp" ->
       let faddr_f = get_or_make_val data scope frameaddress in
       let stacksave_f = get_or_make_val data scope stacksave in
       let setjmp_f = get_or_make_val data scope setjmp in

       let zero = const_null (i32_type data.ctx) in
       let two = const_int (i32_type data.ctx) 2 in
       let jumpbuf = expr_gen scope false (List.hd args) in
       let vp_tp = pointer_type @@ i8_type data.ctx in

       let jumpbuf0 =
         build_in_bounds_gep jumpbuf [| zero; zero |] "" data.bldr in
       let faddr = build_call faddr_f [| zero |] "" data.bldr in
       let () = ignore(build_store faddr jumpbuf0 data.bldr) in
       let jumpbuf2 =
         build_in_bounds_gep jumpbuf [| zero; two |] "" data.bldr in
       let stackaddr = build_call stacksave_f [| |] "" data.bldr in
       let () = ignore(build_store stackaddr jumpbuf2 data.bldr) in

       let jumpbuf_addr = build_bitcast jumpbuf vp_tp "" data.bldr in

       build_call setjmp_f [| jumpbuf_addr |] "" data.bldr

    | "__builtin_longjmp" ->
       let longjmp_f = get_or_make_val data scope longjmp in
       let jumpbuf = expr_gen scope false (List.hd args) in
       let vp_tp = pointer_type @@ i8_type data.ctx in
       let jumpbuf_addr = build_bitcast jumpbuf vp_tp "" data.bldr in
       build_call longjmp_f [| jumpbuf_addr |] "" data.bldr

    | "typestr" ->
       let argstr =
         match args with
         | [{ expr_tp = t }] -> string_of_type t
         | _ -> Report.err_internal __FILE__ __LINE__ "bad typestr arg"
       in
       let str = const_stringz data.ctx argstr in
       let label = label_of_pos (pos_of_cr (List.hd args).expr_cr) in
       define_global ("typestr." ^ label) str data.mdl

    | _ ->
       Report.err_internal __FILE__ __LINE__
         ("Unrecognized builtin: " ^ name)

  (* Generate code for a binary operation and return the register containing
     the result. *)
  and llvm_binop scope op =
    let lhs = op.op_left
    and rhs = Util.the op.op_right
    in

    let standard_op sfcn ufcn ffcn name =
      let lllhs = expr_gen scope true lhs
      and llrhs = expr_gen scope true rhs
      in
      let f = select_for_type sfcn ufcn ffcn lhs.expr_tp in
      f lllhs llrhs name data.bldr
    in

    let assign_op sfcn ufcn ffcn name =
      let lladdr = expr_gen scope false lhs
      and llrhs = expr_gen scope true rhs
      in
      let lllhs = build_load_wrapper lhs.expr_tp lladdr "" in
      let f = select_for_type sfcn ufcn ffcn lhs.expr_tp in
      let llval = f lllhs llrhs name data.bldr in
      ignore(build_store llval lladdr data.bldr);
      llval
    in

    let atomic_rmw rmw_op f name =
      let llrhs = expr_gen scope true rhs in
      let lladdr = expr_gen scope false lhs in
      let llold = build_atomicrmw rmw_op lladdr llrhs
                    AtomicOrdering.SequentiallyConsistent
                    false
                    ""
                    data.bldr
      in
      f llold llrhs name data.bldr
    in

    let nofloat op _ _ _ _ =
      Report.err_internal __FILE__ __LINE__
        ("tried to create an instruction for a float op: " ^ op)
    in

    match op.op_op with
    | OperMinus -> standard_op build_sub build_sub build_fsub ""
    | OperPlus -> standard_op build_add build_add build_fadd ""
    | OperMult -> standard_op build_mul build_mul build_fmul ""
    | OperDiv -> standard_op build_sdiv build_udiv build_fdiv ""
    | OperRemainder -> standard_op build_srem build_urem (nofloat "%") ""
    | OperLShift -> standard_op build_shl build_shl (nofloat "<<") ""
    | OperRShift -> standard_op build_ashr build_lshr (nofloat ">>") ""
    | OperLT -> standard_op (build_icmp Icmp.Slt) (build_icmp Icmp.Ult)
                  (build_fcmp Fcmp.Olt) ""
    | OperGT -> standard_op (build_icmp Icmp.Sgt) (build_icmp Icmp.Ugt)
                  (build_fcmp Fcmp.Ogt) ""
    | OperLTE -> standard_op (build_icmp Icmp.Sle) (build_icmp Icmp.Ule)
                   (build_fcmp Fcmp.Ole) ""
    | OperGTE -> standard_op (build_icmp Icmp.Sge) (build_icmp Icmp.Uge)
                   (build_fcmp Fcmp.Oge) ""
    | OperEquals -> standard_op (build_icmp Icmp.Eq) (build_icmp Icmp.Eq)
                      (build_fcmp Fcmp.Oeq) ""
    | OperNEquals -> standard_op (build_icmp Icmp.Ne) (build_icmp Icmp.Ne)
                       (build_fcmp Fcmp.One) ""
    | OperBitwiseAnd -> standard_op build_and build_and (nofloat "&") ""
    | OperBitwiseXor -> standard_op build_xor build_xor (nofloat "^") ""
    | OperBitwiseOr -> standard_op build_or build_or (nofloat "|") ""
    | OperLogicalAnd ->
       begin
         let label = label_of_pos op.op_pos in
         let lllhs = expr_gen scope true lhs in
         let notlllhs = build_not lllhs "" data.bldr in
         let lbb = Util.the data.curr_bb in
         let rbb = append_block data.ctx (label ^ ".rhs") llfcn in
         data.curr_bb <- Some rbb;
         position_at_end rbb data.bldr;
         let llrhs = expr_gen scope true rhs in
         let phibb = append_block data.ctx (label ^ ".phi") llfcn in
         ignore(build_br phibb data.bldr);
         position_at_end lbb data.bldr;
         ignore(build_cond_br lllhs rbb phibb data.bldr);
         data.curr_bb <- Some phibb;
         position_at_end phibb data.bldr;
         build_phi [(notlllhs, lbb); (llrhs, rbb)] "" data.bldr
       end
    | OperLogicalOr ->
       begin
         let label = label_of_pos op.op_pos in
         let lllhs = expr_gen scope true lhs in
         let lbb = Util.the data.curr_bb in
         let rbb = append_block data.ctx (label ^ ".rhs") llfcn in
         data.curr_bb <- Some rbb;
         position_at_end rbb data.bldr;
         let llrhs = expr_gen scope true rhs in
         let phibb = append_block data.ctx (label ^ ".phi") llfcn in
         ignore(build_br phibb data.bldr);
         position_at_end lbb data.bldr;
         ignore(build_cond_br lllhs phibb rbb data.bldr);
         data.curr_bb <- Some phibb;
         position_at_end phibb data.bldr;
         build_phi [(lllhs, lbb); (llrhs, rbb)] "" data.bldr
       end
    | OperAssign ->
       begin
         match lhs.expr_ast, rhs.expr_ast with
         | ExprStaticStruct _, _ ->
            Report.err_internal __FILE__ __LINE__ "assign of static struct"
         | _, ExprCast (_, _, { expr_ast = ExprStaticStruct (_, members) }) ->
            let base = expr_gen scope false op.op_left in
            let assign n expr =
              let v = expr_gen scope true expr in
              let dest = build_struct_gep base n "" data.bldr in
              ignore(build_store v dest data.bldr)
            in
            let () = List.iteri assign members in
            build_load base "" data.bldr
         | _, ExprStaticArray exprs ->
            let base = expr_gen scope false op.op_left in
            let assign n expr =
              let v = expr_gen scope true expr in
              let i32 = i32_type data.ctx in
              let addr =
                build_in_bounds_gep base
                  [| const_null i32; const_int i32 n |] "" data.bldr
              in
              ignore(build_store v addr data.bldr)
            in
            let () = List.iteri assign exprs in
            build_load base "" data.bldr
         | _ ->
            let lladdr = expr_gen scope false lhs in
            let llval = expr_gen scope true rhs in
            build_store llval lladdr data.bldr
       end
    | OperPlusAssign ->
       if op.op_atomic then
         atomic_rmw AtomicRMWBinOp.Add build_add ""
       else
         assign_op build_add build_add build_fadd ""
    | OperMinusAssign ->
       if op.op_atomic then
         atomic_rmw AtomicRMWBinOp.Sub build_sub ""
       else
         assign_op build_sub build_sub build_fsub ""
    | OperMultAssign ->
       if op.op_atomic then
         Report.err_internal __FILE__ __LINE__
           "Unsupported atomic: *="
       else
         assign_op build_mul build_mul build_fmul ""
    | OperDivAssign ->
       if op.op_atomic then
         Report.err_internal __FILE__ __LINE__
           "Unsupported atomic: /="
       else
         assign_op build_sdiv build_udiv build_fdiv ""
    | OperRemAssign ->
       if op.op_atomic then
         Report.err_internal __FILE__ __LINE__
           "Unsupported atomic: %="
       else
         assign_op build_srem build_urem (nofloat "%=") ""
    | OperLShiftAssign ->
       if op.op_atomic then
         Report.err_internal __FILE__ __LINE__
           "Unsupported atomic: <<="
       else
         assign_op build_shl build_shl (nofloat "<<=") ""
    | OperRShiftAssign ->
       if op.op_atomic then
         Report.err_internal __FILE__ __LINE__
           "Unsupported atomic: >>="
       else
         assign_op build_ashr build_lshr (nofloat ">>=") ""
    | OperBAndAssign ->
       if op.op_atomic then
         atomic_rmw AtomicRMWBinOp.And build_and ""
       else
         assign_op build_and build_and (nofloat "&=") ""
    | OperBXorAssign ->
       if op.op_atomic then
         atomic_rmw AtomicRMWBinOp.Xor build_xor ""
       else
         assign_op build_xor build_xor (nofloat "^=") ""
    | OperBOrAssign ->
       if op.op_atomic then
         atomic_rmw AtomicRMWBinOp.Or build_or ""
       else
         assign_op build_or build_or (nofloat "|=") ""
    | _ -> Report.err_internal __FILE__ __LINE__ "non-binary instr"

  (* Generate code for a unary operation and return the register containing
     the result. *)
  and llvm_unop is_pre scope op =
    match op.op_op with
    | OperIncr ->
       let lladdr = expr_gen scope false op.op_left in
       let llold = build_load_wrapper op.op_left.expr_tp lladdr "" in
       let lltype = type_of llold in
       let llnew =
         if is_integer_type op.op_left.expr_tp then
           build_add llold (const_int lltype 1) "" data.bldr
         else
           build_fadd llold (const_float lltype 1.0) "" data.bldr
       in
       let () = ignore(build_store llnew lladdr data.bldr) in
       if is_pre then llnew
       else llold
    | OperDecr ->
       let lladdr = expr_gen scope false op.op_left in
       let llold = build_load_wrapper op.op_left.expr_tp lladdr "" in
       let lltype = type_of llold in
       let llnew =
         if is_integer_type op.op_left.expr_tp then
           build_sub llold (const_int lltype 1) "" data.bldr
         else
           build_fsub llold (const_float lltype 1.0) "" data.bldr
       in
       let () = ignore(build_store llnew lladdr data.bldr) in
       if is_pre then llnew
       else llold
    | OperAddrOf ->
       let llval = expr_gen scope false op.op_left in
       let zero = const_null (i32_type data.ctx) in
       build_gep llval [| zero |] "addrof" data.bldr
    | OperMinus ->
       let llval = expr_gen scope true op.op_left in
       let f =
         if is_sinteger_type op.op_left.expr_tp then build_nsw_neg
         else if is_uinteger_type op.op_left.expr_tp then build_neg
         else build_fneg
       in
       f llval "" data.bldr
    | OperLogicalNot ->
       let llval = expr_gen scope true op.op_left in
       let lltype = type_of llval in
       let cmp = build_icmp Icmp.Eq llval (const_null lltype) "" data.bldr in
       build_zext cmp lltype "" data.bldr
    | OperBitwiseNot ->
       let llval = expr_gen scope true op.op_left in
       build_not llval "" data.bldr
    | _ -> Report.err_internal __FILE__ __LINE__ "non-unary instr"

  (* Generate code for an expression and return the register containing the
     final value. *)
  and expr_gen scope rval expr =
    match expr.expr_ast with
    | ExprNew (dim, tp, inits) ->
       let lldim = expr_gen scope true dim in
       let lltype = get_or_make_type data tp in
       let lltypesize = Llvm.size_of lltype in
       let llsz = build_mul lldim lltypesize "" data.bldr in
       let llalloc = get_or_make_val data scope "forkscan_malloc" in
       let mem = build_call llalloc [| llsz |] "" data.bldr in
       let cast_type = pointer_type lltype in
       let cast_val = build_bitcast mem cast_type "" data.bldr in

       let () =
         if inits <> [] then
           let fieldset =
             match concrete_of None data.prog.prog_typemap tp with
             | { bare = DefTypeLiteralStruct (_, _, names) } -> names
             | _ -> Report.err_internal __FILE__ __LINE__ "non struct inits"
           in
           let initialize (fieldtok, init) =
             let llinit = expr_gen scope true init in
             let n = whereis fieldset fieldtok.td_text in
             let lladdr = build_struct_gep cast_val n "" data.bldr in
             ignore(build_store llinit lladdr data.bldr)
           in
           List.iter initialize inits
       in

       cast_val
    | ExprFcnCall (name, args, is_spawn) ->
       let llfcn = get_or_make_val data scope name in
       let arg_vals = List.map (expr_gen scope true) args in
       let llcallee =
         match classify_value llfcn with
         | ValueKind.Function -> llfcn
         | _ -> build_load llfcn "" data.bldr
       in
       build_call llcallee (Array.of_list arg_vals) "" data.bldr
    | ExprBuiltinCall (name, args) ->
       builtin_gen scope name args
    | ExprString str ->
       let llstr = const_stringz_wrap data.ctx str in
       define_global ("cstr." ^ (Util.unique_id ())) llstr data.mdl
    | ExprBinary op -> llvm_binop scope op
    | ExprPreUnary op -> llvm_unop true scope op
    | ExprPostUnary op -> llvm_unop false scope op
    | ExprTernaryCond _ ->
       Report.err_internal __FILE__ __LINE__ "expr_gen ternary."
    | ExprVar var ->
       let vaddr = get_or_make_val data scope var in
       begin
         match expr.expr_tp.bare with
         | DefTypeFcn _ -> vaddr
         | DefTypeArray _ -> vaddr
         | _ ->
            if rval then build_load_wrapper expr.expr_tp vaddr ""
            else vaddr
       end
    | ExprLit lit -> const_literal data.lltypesyms lit
    | ExprEnum (_, lit) ->
       const_literal data.lltypesyms lit
    | ExprCast (_, to_type, { expr_ast = ExprNil }) ->
       let zero = const_null (i64_type data.ctx) in
       let lltype = get_or_make_type data to_type in
       build_inttoptr zero lltype "" data.bldr
    | ExprCast (fromtp, totp, expr) ->
       let e = expr_gen scope true expr in
       build_cast data fromtp totp e
    | ExprIndex (base, idx) ->
       let llidx = expr_gen scope true idx in
       let llgep =
         if is_pointer_type base.expr_tp then
           let llbase = expr_gen scope true base in
           build_gep llbase [| llidx |] "" data.bldr
         else
           let zero = const_null (i32_type data.ctx) in
           let llbase = expr_gen scope false base in
           build_in_bounds_gep llbase [| zero; llidx |] "" data.bldr
       in
       if rval then build_load_wrapper expr.expr_tp llgep ""
       else llgep
    | ExprSelectField (base, fid) ->
       let llbase = expr_gen scope false base in
       let rec deref deftp llval =
         match deftp.bare with
         | DefTypeNamed nm ->
            let subtype = Util.the (lookup_symbol data.prog.prog_typemap nm) in
            deref subtype llval
         | DefTypePtr subtype ->
            let llsubval = build_load_wrapper deftp llval "" in
            deref subtype llsubval
         | _ -> deftp, llval
       in
       let deftp, llderef = deref base.expr_tp llbase in
       let n =
         match fid with
         | FieldNumber n -> n
         | FieldName nm ->
            let fields =
              match deftp.bare with
              | DefTypeLiteralStruct (_, _, fnames) -> fnames
              (* | DefTypeLiteralUnion (_, fnames) -> fnames *)
              | _ ->
                 Report.err_internal __FILE__ __LINE__
                   "field of non-struct."
            in
            whereis fields nm
       in
       let lladdr = build_struct_gep llderef n "" data.bldr in
       if rval then build_load_wrapper deftp lladdr ""
       else lladdr
    | ExprStaticStruct (is_packed, fields) ->
       let lltype = get_or_make_type data expr.expr_tp in
       let vname = "static." ^ (Util.unique_id ()) in
       let var = declare_var scope lltype None vname in

       let initialize n expr =
         let llval = expr_gen scope true expr in
         let dest = build_struct_gep var n "" data.bldr in
         ignore(build_store llval dest data.bldr)
       in

       let () = List.iteri initialize fields in
       build_load var "" data.bldr
    | ExprStaticArray _ ->
       Report.err_internal __FILE__ __LINE__ "expr_gen static array."
    | ExprType _ ->
       Report.err_internal __FILE__ __LINE__ "expr_gen type."
    | ExprTypeString _ ->
       Report.err_internal __FILE__ __LINE__ "expr_gen type string."
    | ExprNil ->
       Report.err_internal __FILE__ __LINE__ "expr_gen nil."
    | ExprWildcard ->
       Report.err_internal __FILE__ __LINE__ "expr_gen wildcard."
  in

  let spawn_gen scope expr =
    let spawn ret_maybe name args =
      let label = label_of_pos (pos_of_cr expr.expr_cr) in
      let sync_region = get_or_make_sync_region scope in

      let lladdr =
        match ret_maybe with
        | None -> None
        | Some ret -> Some (expr_gen scope false ret)
      in

      let llargs = List.map (expr_gen scope true) args in
      let detach_bb = append_block data.ctx (label ^ ".detach") llfcn in
      let cont_bb = append_block data.ctx (label ^ ".cont") llfcn in
      ignore(build_detach detach_bb cont_bb sync_region data.bldr);

      set_curr_bb detach_bb;

      let fcn = get_or_make_val data scope name in
      let llcallee =
        match classify_value fcn with
        | ValueKind.Function -> fcn
        | _ -> build_load fcn "" data.bldr
      in
      let llv = build_call llcallee (Array.of_list llargs) "" data.bldr in
      if lladdr <> None then
        ignore(build_store llv (Util.the lladdr) data.bldr);
      ignore(build_reattach cont_bb sync_region data.bldr);

      set_curr_bb cont_bb
    in
    match expr with
    | { expr_ast = ExprFcnCall (name, args, true) } ->
       spawn None name args
    | { expr_ast = ExprBinary ({
                         op_left = ret;
                         op_right =
                           Some { expr_ast =
                                    ExprFcnCall (name, args, true) }}) } ->
       spawn (Some ret) name args
    | _ ->
       Report.err_internal __FILE__ __LINE__ "unknown spawn format."
  in

  let rec stmt_gen scope = function
    | Import _ -> ()
    | StmtExpr (_, expr) ->
       if is_spawn_expr expr then
         spawn_gen scope expr
       else
         ignore(expr_gen scope true expr)
    | Block (_, body) ->
       let subscope = push_symtab_scope scope in
       List.iter (stmt_gen subscope) body
    | DeclFcn _ ->
       Report.err_internal __FILE__ __LINE__ "local decls not implemented."
    | DefFcn _ ->
       Report.err_internal __FILE__ __LINE__ "local defs not implemented."
    | VarDecl (_, vtoks, [], deftype, _) ->
       let lltype = get_or_make_type data deftype in
       List.iter
         (fun v -> ignore(declare_var scope lltype None v.td_text))
         vtoks
    | VarDecl (_, vtoks, inits, deftype, _) ->
       let lltype = get_or_make_type data deftype in
       if is_spawn_expr (List.hd inits) then
         let var = (List.hd vtoks).td_text in
         let () = ignore(declare_var scope lltype None var) in
         spawn_gen scope (List.hd inits)
       else
         List.iter2
           (fun v i -> ignore(declare_var scope lltype (Some i) v.td_text))
           vtoks inits
    | InlineStructVarDecl (_, fields, (_, rhs)) ->
       (* FIXME: This structure should allow recursive unpacking.  E.g.,
          var { foo, { bar, baz } } = quux();
        *)
       let tmp_var_name = "unpack." ^ (Util.unique_id ()) in
       let tmp_type = get_or_make_type data rhs.expr_tp in
       let tmp_var = declare_var scope tmp_type (Some rhs) tmp_var_name in
       List.iteri
         (fun n (_, name, deftype) ->
           let lltype = get_or_make_type data deftype in
           let llvar = declare_var scope lltype None name in
           let lladdr = build_struct_gep tmp_var n "" data.bldr in
           let v = build_load_wrapper deftype lladdr "" in
           ignore(build_store v llvar data.bldr))
         fields
    | TransactionBlock _ ->
       Report.err_internal __FILE__ __LINE__ "not implemented"
    | IfStmt (pos, cond, then_br, else_br_maybe) ->
       let llcond = expr_gen scope true cond in
       let cond_bb = Util.the data.curr_bb in

       let label = label_of_pos pos in
       let then_begin, then_end =
         make_block (label ^ ".then") scope then_br in

       begin
         match else_br_maybe with
         | None ->
            let join_bb = append_block data.ctx (label ^ ".join") llfcn in
            let () = if bb_needs_exit then_end then
                       ignore(build_br join_bb data.bldr)
            in
            let () = set_curr_bb cond_bb in
            let () =
              ignore(build_cond_br llcond then_begin join_bb data.bldr)
            in
            set_curr_bb join_bb
         | Some (epos, else_br) ->
            let elabel = label_of_pos epos in
            let else_begin, else_end =
              make_block (elabel ^ ".else") scope else_br in
            let () = set_curr_bb cond_bb in
            let () =
              ignore(build_cond_br llcond then_begin else_begin data.bldr)
            in
              if bb_needs_exit then_end || bb_needs_exit else_end then
                let join_bb = append_block data.ctx (label ^ ".join") llfcn in
                if bb_needs_exit then_end then
                  begin
                    set_curr_bb then_end;
                    ignore(build_br join_bb data.bldr)
                  end;
                if bb_needs_exit else_end then
                  begin
                    set_curr_bb else_end;
                    ignore(build_br join_bb data.bldr)
                  end;
                set_curr_bb join_bb
       end
    | ForLoop (pos, false, init_maybe, (_, cond), iter_maybe, _, body) ->
       let label = label_of_pos pos in
       let saved_break = save_break_bb () in
       let subscope = push_symtab_scope scope in
       let () = maybe_apply (stmt_gen subscope) init_maybe in
       let cond_bb = append_block data.ctx (label ^ ".cond") llfcn in
       let () = ignore(build_br cond_bb data.bldr) in
       let () = set_curr_bb cond_bb in
       let cond_register = expr_gen subscope true cond in
       let cond_end = Util.the data.curr_bb in

       let iter_bb =
         match iter_maybe with
         | None -> cond_bb
         | Some (_, iter) ->
            let iter_bb = append_block data.ctx (label ^ ".iter") llfcn in
            let () = set_curr_bb iter_bb in
            let () = ignore(expr_gen subscope true iter) in
            let () = ignore(build_br cond_bb data.bldr) in
            iter_bb
       in

       let () = push_continue_bb iter_bb in

       let body_scope = push_symtab_scope subscope in
       let body_begin, body_end =
         make_block (label ^ ".forbody") body_scope body
       in

       let () = if bb_needs_exit body_end then
                  ignore(build_br iter_bb data.bldr)
       in
       let () = set_curr_bb cond_end in
       let () =
         if provably_always_true cond then
           let () = ignore(build_br body_begin data.bldr) in
           match !break_bb with
           | None -> ()
           | Some bb -> set_curr_bb bb
         else
           let next_bb =
             match !break_bb with
             | None -> append_block data.ctx (label ^ ".next") llfcn
             | Some bb -> bb
           in
           ignore(build_cond_br cond_register body_begin next_bb data.bldr);
           set_curr_bb next_bb
       in

       let () = pop_continue_bb () in
       break_bb := saved_break
    | ForLoop _ ->
       Report.err_internal __FILE__ __LINE__ "parfor not implemented"
    | WhileLoop (pos, is_pre, cond, body) ->
       let label = label_of_pos pos in
       let saved_break = save_break_bb () in
       let begin_bb = Util.the data.curr_bb in
       let cond_bb = append_block data.ctx (label ^ ".cond") llfcn in
       let () = set_curr_bb cond_bb in
       let cond_register = expr_gen scope true cond in
       let cond_end = Util.the data.curr_bb in

       let () = push_continue_bb cond_bb in

       let subscope = push_symtab_scope scope in
       let body_begin, body_end =
         make_block (label ^ ".whilebody") subscope body
       in

       let () = if bb_needs_exit body_end then
                  ignore(build_br cond_bb data.bldr)
       in

       let () = set_curr_bb begin_bb in
       let () = if is_pre then
                  ignore(build_br cond_bb data.bldr)
                else
                  ignore(build_br body_begin data.bldr)
       in
       let () = set_curr_bb cond_end in
       let () =
         if provably_always_true cond then
           let () = ignore(build_br body_begin data.bldr) in
           match !break_bb with
           | None -> ()
           | Some bb -> set_curr_bb bb
         else
           let next_bb =
             match !break_bb with
             | None -> append_block data.ctx (label ^ ".next") llfcn
             | Some bb -> bb
           in
           ignore(build_cond_br cond_register body_begin next_bb data.bldr);
           set_curr_bb next_bb
       in

       let () = pop_continue_bb () in
       break_bb := saved_break
    | SwitchStmt (pos, expr, cases) ->
       build_general_switch scope pos expr cases
    | Return (_, expr) ->
       let llexpr = expr_gen scope true expr in
       ignore(build_ret llexpr data.bldr)
    | ReturnVoid _ ->
       ignore(build_ret_void data.bldr)
    | TypeDecl _ ->
       Report.err_internal __FILE__ __LINE__ "local typedecls not implemented"
    | Label (_, str) ->
       let label = "label." ^ str in
       let bb = get_or_make_bb label in
       let () = ignore(build_br bb data.bldr) in
       set_curr_bb bb
    | Goto (_, str) ->
       let label = "label." ^ str in
       let bb = get_or_make_bb label in
       ignore(build_br bb data.bldr)
    | Break pos ->
       let () =
         if !break_bb = None then
           let label = (label_of_pos pos) ^ ".out" in
           break_bb := Some (append_block data.ctx label llfcn)
       in
       ignore(build_br (Util.the !break_bb) data.bldr)
    | Continue _ ->
       ignore(build_br (List.hd !continue_bb) data.bldr)
    | Sync pos ->
       let label = (label_of_pos pos) ^ ".postsync" in
       let ps_bb = append_block data.ctx label llfcn in
       let sr = get_or_make_sync_region scope in
       let () = ignore(build_sync ps_bb sr data.bldr) in
       set_curr_bb ps_bb

  and build_general_switch scope pos expr cases =
    let llexpr = expr_gen scope true expr in

    let follow = ref None in
    let get_follow () =
      let label = label_of_pos pos in
      if !follow = None then
        follow := Some (append_block data.ctx (label ^ ".follow") llfcn);
      Util.the !follow
    in

    let process_case
          (prev_maybe, tails)
          (p, is_fall_through, pattern, body) =
      let label = label_of_pos pos in
      let root_bb = Util.the data.curr_bb in
      let fail_bb = ref None in

      let get_fail () =
        if !fail_bb = None then
          fail_bb := Some (append_block data.ctx (label ^ ".fail") llfcn);
        Util.the !fail_bb
      in

      let rec build_cmp prev tp e pattern =
        match pattern.expr_ast with
        | ExprWildcard ->
           prev
        | ExprLit lit
        | ExprEnum (_, lit) ->
           let label = label_of_pos @@ pos_of_cr pattern.expr_cr in
           let v = const_literal data.lltypesyms lit in
           let f = select_for_type (build_icmp Icmp.Eq) (build_icmp Icmp.Eq)
                     (build_fcmp Fcmp.Oeq) pattern.expr_tp
           in
           let result = f e v "" data.bldr in
           let succ_bb = append_block data.ctx (label ^ ".succ") llfcn in
           let fail_bb = get_fail () in
           let () = ignore(build_cond_br result succ_bb fail_bb data.bldr) in
           let () = set_curr_bb succ_bb in
           succ_bb
        | ExprStaticStruct (_, mpatterns) ->
           let rec check_member prev n = function
             | [] -> prev
             | mpattern :: rest ->
                let member = build_extractvalue e n "" data.bldr in
                let next_prev =
                  build_cmp prev mpattern.expr_tp member mpattern
                in
                check_member next_prev (n + 1) rest
           in
           check_member prev 0 mpatterns
        | ExprString strpattern ->
           let len = String.length strpattern in
           let rec compare_char prev n =
             if len > n then
               let lln = const_int (i32_type data.ctx) n in
               let llnth =
                 if is_pointer_type tp then
                   let lladdr = build_gep e [| lln |] "" data.bldr in
                   build_load_wrapper tp lladdr ""
                 else
                   build_in_bounds_gep e [| lln |] "" data.bldr
               in
               let cpattern =
                 { expr_cr = pattern.expr_cr;
                   expr_tp = maketype None (DefTypePrimitive PrimI8);
                   expr_ast = ExprLit (LitI8 strpattern.[n]);
                 }
               in
               let next_prev = build_cmp prev tp llnth cpattern in
               compare_char next_prev (n + 1)
             else prev
           in
           compare_char prev 0
        | ExprStaticArray elements ->
           let rec check_element prev n = function
             | [] -> prev
             | el :: rest ->
                let lln = const_int (i32_type data.ctx) n in
                let lladdr = build_gep e [| lln |] "" data.bldr in
                let llval = build_load_wrapper tp lladdr "" in
                let next_prev = build_cmp prev el.expr_tp llval el in
                check_element next_prev (n + 1) rest
           in
           check_element prev 0 elements
        | _ ->
           Report.err_internal __FILE__ __LINE__ "unimplemented switch."
      in

      let body_bb = build_cmp root_bb expr.expr_tp llexpr pattern in
      let tails =
        (* Get the new list of trailing basic blocks from the set of cases
           visited so far. *)
        if is_fall_through then
          if prev_maybe = None then tails
          else
            begin
              set_curr_bb (Util.the prev_maybe);
              ignore(build_br body_bb data.bldr);
              tails
            end
        else
          if prev_maybe = None then tails
          else (Util.the prev_maybe) :: tails
      in
      set_curr_bb body_bb;
      List.iter (stmt_gen scope) body;
      let next_bb =
        if bb_needs_exit (Util.the data.curr_bb) then
          Some (Util.the data.curr_bb)
        else
          None
      in
      if !fail_bb <> None then
        set_curr_bb (Util.the !fail_bb);
      next_bb, tails
    in

    let prev_maybe, tails = List.fold_left process_case (None, []) cases in

    let do_next warn bb =
      if bb_needs_exit bb then
        begin
          if warn then
            Report.warn_incomplete_switch pos;
          set_curr_bb bb;
          ignore(build_br (get_follow ()) data.bldr)
        end
    in

    do_next false (Util.the data.curr_bb);
    if prev_maybe <> None then do_next true (Util.the prev_maybe);
    List.iter (do_next false) tails;
    if !follow <> None then
      set_curr_bb (Util.the !follow)

  and make_block label scope stmts =
    let begin_bb = append_block data.ctx label llfcn in
    set_curr_bb begin_bb;
    let subscope = push_symtab_scope scope in
    List.iter (stmt_gen subscope) stmts;
    begin_bb, Util.the data.curr_bb
  in

  List.iter (stmt_gen fcn_scope) fcn_body

let build_function data = function
  | DefFcn (_, _, _, name,
            { bare = DefTypeFcn (ptypes, _, _) },
            defparams,
            body) ->
     let local = make_symtab () in
     let llfcn = get_or_make_val data local name in
     let llparams = params llfcn in
     let entry = append_block data.ctx "entry" llfcn in
     let () = data.curr_bb <- Some entry in
     let () = position_at_end entry data.bldr in

     (* Store the arguments into local variables. *)
     let () = List.iteri
                (fun n (tp, (_, var)) ->
                  let lltp = get_or_make_type data tp in
                  let llval = build_alloca lltp var data.bldr in
                  add_symbol local var llval;
                  ignore(build_store llparams.(n) llval data.bldr))
                (List.combine ptypes defparams)
     in
     let init_bb = append_block data.ctx (name ^ ".init") llfcn in
     begin
       data.curr_bb <- Some init_bb;
       position_at_end init_bb data.bldr;
       ir_gen data llfcn local entry body;

       (* Hook up the entry to the init block. *)
       position_at_end entry data.bldr;
       ignore(build_br init_bb data.bldr);
       if not !Config.codegen_debug then
         Llvm_analysis.assert_valid_function llfcn
     end

  | _ -> ()

let ir_of_ast module_name prog =
  let ctx  = global_context () in
  let mdl  = create_module ctx module_name in
  let ()   = set_target_triple (Target.default_triple ()) mdl in
  let bldr = builder ctx in

  let lltypesyms = basic_lltypes ctx in

  let data =
    { ctx  = ctx;
      mdl  = mdl;
      bldr = bldr;
      prog = prog;

      difile = None;
      dib  = None;

      globalsyms = make_symtab ();
      globalvals = make_symtab ();
      lltypesyms = lltypesyms;
      curr_bb = None;
    }
  in

  make_module_flags data;
  if !Config.debug_symbols then debug_sym_preamble data module_name;
  List.iter (gather_global data) prog.prog_ast;
  List.iter (build_function data) prog.prog_ast;

  mdl
