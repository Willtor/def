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
  | StuIdent tok -> tok
  | _ ->
     Error.fatal_error "FIXME: Need suitable error. ident_token_of"

let define stubindings = function
  | [(StuIdent id); value] ->
     let resolved_value = eval_stu stubindings value in
     let binding = BBStu resolved_value in
     add_symbol stubindings id.td_text binding
  | [(StuInt32 _) ; _] ->
     let () = prerr_endline "hey now int32" in
     Error.fatal_error "FIXME: Need suitable error."
  | _ ->
     let () = prerr_endline "hey now" in
     Error.fatal_error "FIXME: Need suitable error."

let master_lexer depth stubindings lexbuf =
  let store at sexpr = Some (STU_EXPR (at, sexpr)) in

  let store_or_stash at sexpr =
    match sexpr with
    | StuSexpr (_, (StuIdent fcn) :: rest) ->
       begin
         match fcn.td_text with
         | "define" ->
            (define stubindings rest; None)
         | _ ->
            store at sexpr
       end
    | _ ->
       Error.fatal_error "FIXME: Need suitable error.  Malformed s-expr."
  in

  let rec base_deflex () =
    try deflex lexbuf
    with BeginStu at ->
      match base_stulex at with
      | None -> base_deflex ()
      | Some token -> token

  and base_stulex at =
    let rec stuparse pos accum =
      let continue v = stuparse pos (v :: accum) in
      match stulex lexbuf with
      | StuLexOpen tok ->
         continue @@ stuparse tok.td_pos []
      | StuLexClose _ ->
         StuSexpr (pos, List.rev accum)
      | StuLexString (tok, str) ->
         continue @@ StuString (tok.td_pos, str)
      | StuLexBool (tok, bool) ->
         continue @@ StuBool (tok.td_pos, bool)
      | StuLexChar (tok, n) ->
         continue @@ StuChar (tok.td_pos, n)
      | StuLexUChar (tok, n) ->
         continue @@ StuUChar (tok.td_pos, n)
      | StuLexInt16 (tok, n) ->
         continue @@ StuInt16 (tok.td_pos, n)
      | StuLexUInt16 (tok, n) ->
         continue @@ StuUInt16 (tok.td_pos, n)
      | StuLexInt32 (tok, n) ->
         continue @@ StuInt32 (tok.td_pos, n)
      | StuLexUInt32 (tok, n) ->
         continue @@ StuUInt32 (tok.td_pos, n)
      | StuLexInt64 (tok, n) ->
         continue @@ StuInt64 (tok.td_pos, n)
      | StuLexUInt64 (tok, n) ->
         continue @@ StuUInt64 (tok.td_pos, n)
      | StuLexFloat32 (tok, n) ->
         continue @@ StuFloat32 (tok.td_pos, n)
      | StuLexFloat64 (tok, n) ->
         continue @@ StuFloat64 (tok.td_pos, n)
      | StuLexIdent tok ->
         continue @@ StuIdent tok
    in
    match stulex lexbuf with
    | StuLexOpen tok ->
       let sexpr = stuparse tok.td_pos [] in
       store_or_stash at sexpr
    | StuLexClose tok ->
       Error.err_pos "No matching open square bracket." tok.td_pos
    | StuLexIdent tok ->
       store at (StuIdent tok)
    | _ ->
       Error.fatal_error "Not a function or DEF value."

  in
  base_deflex ()

(** Generate a parse tree from the given input channel/filename. *)
let from_in_channel filename channel stubindings =
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
  try defparse (master_lexer 0 stubindings) lexbuf
  with _ ->
    let pos = lexeme_start_p lexbuf in
    let posstr = Error.format_position pos in
    let srcstr = Error.show_source pos in
    Error.fatal_error (posstr ^ "\n  Syntax error.\n" ^ srcstr ^ "\n")
