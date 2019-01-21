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

open Deflex
open Defparse
open Lexing
open Parsetree
open Isminterp
open Util

let ident_token_of = function
  | IsmIdent tok -> tok
  | _ ->
     Error.fatal_error "FIXME: Need suitable error. ident_token_of"

let define ismbindings = function
  | [(IsmIdent id); value] ->
     let resolved_value = eval_ism ismbindings value in
     let binding = BBIsm resolved_value in
     add_symbol ismbindings id.td_text binding
  | [(IsmSexpr (_, fname :: profile)); body] ->
     let get_variables = function
       | IsmIdent tok -> tok
       | _ ->
          Error.fatal_error "FIXME: Need suitable error.  Bad profile"
     in
     let variables = List.map get_variables (fname :: profile) in
     let lambda = BBLambda (List.tl variables, ismbindings, body) in
     add_symbol ismbindings (List.hd variables).td_text lambda
  | [(IsmInt32 _) ; _] ->
     let () = prerr_endline "hey now int32" in
     Error.fatal_error "FIXME: Need suitable error."
  | _ ->
     let () = prerr_endline "hey now" in
     Error.fatal_error "FIXME: Need suitable error."

let rec master_lexer preseed ismbindings lexbuf =
  let store_expr at sexpr = Some (ISM_EXPR (at, sexpr)) in

  let store_stmts sexpr = Some (ISM_STMTS sexpr) in

  let store_delayed_stmts at sexpr = Some (ISM_DELAYED_STMTS (at, sexpr)) in

  let store_ident at sexpr = Some (ISM_IDENT (at, sexpr)) in

  let store_or_stash at sexpr =
    match sexpr with
    | IsmSexpr (pos, (IsmIdent fcn) :: rest) ->
       let extract_single_arg fcn =
         match rest with
         | [ arg ] -> arg
         | _ -> Error.fatal_error ("expected single arg for " ^ fcn)
       in
       begin
         match fcn.td_text with
         (* Special forms:
            FIXME: Identifying special forms should be handled by the lexer. *)
         | "define" ->
            (define ismbindings rest; None)
         | "emit-expr" ->
            store_expr at (extract_single_arg "emit-expr")
         | "emit-ident" ->
            store_ident at (extract_single_arg "emit-ident")
         | "emit-stmts" ->
            store_delayed_stmts at (extract_single_arg "emit-stmts")
         | _ ->
            match eval_ism ismbindings sexpr with
            | IsmDefStmts stmts ->
               store_stmts stmts
            | IsmDefExpr (tok, expr) ->
               store_expr tok (IsmDefExpr (tok, expr))
            | IsmDefIdent (pos, id) ->
               let tok = { td_pos = pos;
                           td_text = id;
                           td_noncode = []
                         }
               in
               Some (IDENT tok)
            | v ->
               store_expr at v
       end
    | IsmDefStmts stmts ->
       store_stmts stmts
    | IsmDefExpr (tok, expr) ->
       store_expr tok (IsmDefExpr (tok, expr))
    | _ ->
       Error.fatal_error "FIXME: Need suitable error.  Malformed s-expr."
  in

  let rec base_deflex () =
    try deflex lexbuf
    with BeginIsm at ->
      match base_stulex at with
      | None -> base_deflex ()
      | Some token -> token

  and base_stulex at =
    let rec ismparse pos accum =
      let continue v = ismparse pos (v :: accum) in
      match ismlex lexbuf with
      | IsmLexOpen tok ->
         continue @@ ismparse tok.td_pos []
      | IsmLexClose _ ->
         IsmSexpr (pos, List.rev accum)
      | IsmLexQuote tok ->
         let rec get_quoted_value = function
           | IsmLexOpen tok -> ismparse tok.td_pos []
           | IsmLexClose tok -> Ismerr.err_syntax tok.td_pos tok.td_text
           | IsmLexQuote tok ->
              return_quoted_value tok @@ get_quoted_value (ismlex lexbuf)
           | IsmLexString (tok, str) -> IsmString (tok.td_pos, str)
           | IsmLexBool (tok, bool) -> IsmBool (tok.td_pos, bool)
           | IsmLexChar (tok, n) -> IsmChar (tok.td_pos, n)
           | IsmLexUChar (tok, n) -> IsmUChar (tok.td_pos, n)
           | IsmLexInt16 (tok, n) -> IsmInt16 (tok.td_pos, n)
           | IsmLexUInt16 (tok, n) -> IsmUInt16 (tok.td_pos, n)
           | IsmLexInt32 (tok, n) -> IsmInt32 (tok.td_pos, n)
           | IsmLexUInt32 (tok, n) -> IsmUInt32 (tok.td_pos, n)
           | IsmLexInt64 (tok, n) -> IsmInt64 (tok.td_pos, n)
           | IsmLexUInt64 (tok, n) -> IsmUInt64 (tok.td_pos, n)
           | IsmLexFloat32 (tok, n) -> IsmFloat32 (tok.td_pos, n)
           | IsmLexFloat64 (tok, n) -> IsmFloat64 (tok.td_pos, n)
           | IsmLexIdent tok -> IsmIdent tok
         and return_quoted_value tok expr =
           let fcn =
             { td_pos = tok.td_pos;
               td_text = "quote";
               td_noncode = tok.td_noncode
             }
           in
           IsmSexpr (tok.td_pos, [ IsmIdent fcn; expr ])
         in
         continue @@
           return_quoted_value tok @@ get_quoted_value (ismlex lexbuf)
      | IsmLexString (tok, str) ->
         continue @@ IsmString (tok.td_pos, str)
      | IsmLexBool (tok, bool) ->
         continue @@ IsmBool (tok.td_pos, bool)
      | IsmLexChar (tok, n) ->
         continue @@ IsmChar (tok.td_pos, n)
      | IsmLexUChar (tok, n) ->
         continue @@ IsmUChar (tok.td_pos, n)
      | IsmLexInt16 (tok, n) ->
         continue @@ IsmInt16 (tok.td_pos, n)
      | IsmLexUInt16 (tok, n) ->
         continue @@ IsmUInt16 (tok.td_pos, n)
      | IsmLexInt32 (tok, n) ->
         continue @@ IsmInt32 (tok.td_pos, n)
      | IsmLexUInt32 (tok, n) ->
         continue @@ IsmUInt32 (tok.td_pos, n)
      | IsmLexInt64 (tok, n) ->
         continue @@ IsmInt64 (tok.td_pos, n)
      | IsmLexUInt64 (tok, n) ->
         continue @@ IsmUInt64 (tok.td_pos, n)
      | IsmLexFloat32 (tok, n) ->
         continue @@ IsmFloat32 (tok.td_pos, n)
      | IsmLexFloat64 (tok, n) ->
         continue @@ IsmFloat64 (tok.td_pos, n)
      | IsmLexIdent tok ->
         if accum = [] && tok.td_text = "parse-stmts" then
           let subbindings = push_symtab_scope ismbindings in
           let stmts =
             match master_parser (Some ISM_STATEMENTS) subbindings lexbuf with
             | [PTS_ISM_Stmts stmts] -> stmts
             | _ -> Ismerr.internal
                      "Tried to parse statements and got something else back."
           in
           IsmDefStmts stmts
         else if accum = [] && tok.td_text = "parse-expr" then
           let subbindings = push_symtab_scope ismbindings in
           let expr =
             match master_parser (Some ISM_EXPRESSION) subbindings lexbuf with
             | [PTS_ISM_Expr expr] -> expr
             | _ -> Ismerr.internal
                      "Tried to parse expression and got something else back."
           in
           IsmDefExpr (tok, expr)
         else
           continue @@ IsmIdent tok
    in
    match ismlex lexbuf with
    | IsmLexOpen tok ->
       let sexpr = ismparse tok.td_pos [] in
       store_or_stash at sexpr
    | IsmLexClose tok ->
       Error.err_pos "No matching open square bracket." tok.td_pos
    | IsmLexIdent tok ->
       begin
         match lookup_symbol ismbindings tok.td_text with
         | None ->
            Error.fatal_error ("no binding; need suitable error " ^ tok.td_text)
         | Some (BBIsm (IsmDefStmts stmts)) ->
            store_stmts stmts
         | Some (BBIsm (IsmDefExpr (tok, expr))) ->
            store_expr at (IsmDefExpr (tok, expr))
         | Some (BBIsm (IsmDefIdent (pos, id))) ->
            let tok = { td_pos = pos;
                        td_text = id;
                        td_noncode = []
                      }
            in
            Some (IDENT tok)
         | Some _ ->
            store_expr at (IsmIdent tok)
       end
    | _ ->
       Error.fatal_error "Not a function or DEF value."

  in
  if !preseed = None then
    base_deflex ()
  else
    let v = Util.the !preseed in
    preseed := None;
    v

and master_parser preseed ismbindings lexbuf =
  try defparse (master_lexer (ref preseed) ismbindings) lexbuf
  with _ ->
    let pos = lexeme_start_p lexbuf in
    let posstr = Error.format_position pos in
    let srcstr = Error.show_source pos in
    Error.fatal_error (posstr ^ "\n  Syntax error.\n" ^ srcstr ^ "\n")

(** Generate a parse tree from the given input channel/filename. *)
let from_in_channel filename channel ismbindings =
  let lexbuf = Lexing.from_channel channel in
  lexbuf.lex_start_p <-
    { pos_fname = filename;
      pos_lnum = lexbuf.lex_start_p.pos_lnum;
      pos_bol = lexbuf.lex_start_p.pos_bol;
      pos_cnum = lexbuf.lex_start_p.pos_cnum
    };
  lexbuf.lex_curr_p <-
    { pos_fname = filename;
      pos_lnum = lexbuf.lex_curr_p.pos_lnum;
      pos_bol = lexbuf.lex_curr_p.pos_bol;
      pos_cnum = lexbuf.lex_curr_p.pos_cnum
    };
  master_parser None ismbindings lexbuf
