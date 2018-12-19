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
  | [(IsmInt32 _) ; _] ->
     let () = prerr_endline "hey now int32" in
     Error.fatal_error "FIXME: Need suitable error."
  | _ ->
     let () = prerr_endline "hey now" in
     Error.fatal_error "FIXME: Need suitable error."

let master_lexer depth ismbindings lexbuf =
  let store at sexpr = Some (ISM_EXPR (at, sexpr)) in

  let store_or_stash at sexpr =
    match sexpr with
    | IsmSexpr (_, (IsmIdent fcn) :: rest) ->
       begin
         match fcn.td_text with
         | "define" ->
            (define ismbindings rest; None)
         | _ ->
            store at sexpr
       end
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
         continue @@ IsmIdent tok
    in
    match ismlex lexbuf with
    | IsmLexOpen tok ->
       let sexpr = ismparse tok.td_pos [] in
       store_or_stash at sexpr
    | IsmLexClose tok ->
       Error.err_pos "No matching open square bracket." tok.td_pos
    | IsmLexIdent tok ->
       store at (IsmIdent tok)
    | _ ->
       Error.fatal_error "Not a function or DEF value."

  in
  base_deflex ()

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
  try defparse (master_lexer 0 ismbindings) lexbuf
  with _ ->
    let pos = lexeme_start_p lexbuf in
    let posstr = Error.format_position pos in
    let srcstr = Error.show_source pos in
    Error.fatal_error (posstr ^ "\n  Syntax error.\n" ^ srcstr ^ "\n")
