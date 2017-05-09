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

{
  open Defparse
  open Lexing
  open Util

  let proc_newlines lexbuf =
    String.iter (fun c -> if c = '\n' then new_line lexbuf)

  let remove_suffix s n =
    let length = String.length s in
    String.sub s 0 (length - n)

  let get_i8_of_str pos str =
    let c = String.get str 1 in
    if c = '\\' then
      match String.get str 2 with
      | 'n' -> '\n'   (* Newline *)
      | 'r' -> '\r'   (* Carriage return *)
      | 't' -> '\t'   (* Tab *)
      | '\\' -> '\\'  (* Backslash *)
      | '\'' -> '\''  (* Single-quote *)
      | _ -> Report.err_bad_escaped_char pos c
    else c

  let remove_quotes s = String.sub s 1 ((String.length s) - 2)

  let documentation : string option ref = ref None

  let set_doc d =
    documentation := Some d

  let clear_doc () =
    documentation := None

  let curr_position lexbuf =
    clear_doc ();
    lexeme_start_p lexbuf
}

rule deflex = parse
| [' ' '\t']+
    { deflex lexbuf }
| '\n' { new_line lexbuf; deflex lexbuf }
| "/**"([^'*']*"*"+[^'*' '/'])*[^'*']*"*"+"/" as comment
    { proc_newlines lexbuf comment;
      set_doc comment;
      deflex lexbuf }
| "/*"([^'*']*"*"+[^'*' '/'])*[^'*']*"*"+"/" as comment
    { proc_newlines lexbuf comment;
      deflex lexbuf }
| "//"[^'\n']* { deflex lexbuf }
| "type" { TYPE (curr_position lexbuf) }
| "typedef" { TYPEDEF (curr_position lexbuf) }
| "begin" { BEGIN (curr_position lexbuf) }
| "end" { END (curr_position lexbuf) }
| "export"
    { let doc = !documentation in
      clear_doc();
      EXPORT (curr_position lexbuf, doc)
    }
| "opaque" { OPAQUE (curr_position lexbuf) }
| "def" { DEF (curr_position lexbuf) }
| "decl" { DECL (curr_position lexbuf) }
| "var" { VAR (curr_position lexbuf) }
| "return" { RETURN (curr_position lexbuf) }
| "if" { IF (curr_position lexbuf) }
| "then" { THEN (curr_position lexbuf) }
| "elif" { ELIF (curr_position lexbuf) }
| "else" { ELSE (curr_position lexbuf) }
| "fi"   { FI (curr_position lexbuf) }
| "for" { FOR (curr_position lexbuf) }
| "while" { WHILE (curr_position lexbuf) }
| "do" { DO (curr_position lexbuf) }
| "done" { DONE (curr_position lexbuf) }
| "cast" { CAST (curr_position lexbuf) }
| "as" { AS (curr_position lexbuf) }
| "goto" { GOTO (curr_position lexbuf) }
| "break" { BREAK (curr_position lexbuf) }
| "continue" { CONTINUE (curr_position lexbuf) }
| "new" { NEW (curr_position lexbuf) }
| "delete" { DELETE (curr_position lexbuf) }
| "retire" { RETIRE (curr_position lexbuf) }
| "nil" { NIL (curr_position lexbuf) }
| "true" { LITERALBOOL (curr_position lexbuf, true) }
| "false" { LITERALBOOL (curr_position lexbuf, false) }
| ['"'][^'"']*['"'] as str { STRING (curr_position lexbuf,
                                     remove_quotes str) }

(* Integers. *)

| "'"[^'\\' '\'']|("\\"_)"'" as cstr
    { let pos = curr_position lexbuf in
      LITERALI8 (pos, get_i8_of_str pos cstr) }
| ['0'-'9']+"I64" as istr
| "0x"['0'-'9' 'A'-'F' 'a'-'f']+"I64" as istr
    { LITERALI64 (curr_position lexbuf,
                  Int64.of_string (remove_suffix istr 3)) }
| ['0'-'9']+"U64" as istr
| "0x"['0'-'9' 'A'-'F' 'a'-'f']+"U64" as istr
    { LITERALU64 (curr_position lexbuf,
                  Int64.of_string (remove_suffix istr 3)) }
| ['0'-'9']+"I32" as istr
| "0x"['0'-'9' 'A'-'F' 'a'-'f']+"I32" as istr
    { LITERALI32 (curr_position lexbuf,
                  Int32.of_string (remove_suffix istr 3)) }
| ['0'-'9']+"U32" as istr
| "0x"['0'-'9' 'A'-'F' 'a'-'f']+"U32" as istr
    { LITERALU32 (curr_position lexbuf,
                  Int32.of_string (remove_suffix istr 3)) }
| ['0'-'9']+"I16" as istr
| "0x"['0'-'9' 'A'-'F' 'a'-'f']+"I16" as istr
    { LITERALI16 (curr_position lexbuf,
                  Int32.of_string (remove_suffix istr 3)) }
| ['0'-'9']+"U16" as istr
| "0x"['0'-'9' 'A'-'F' 'a'-'f']+"U16" as istr
    { LITERALU16 (curr_position lexbuf,
                  Int32.of_string (remove_suffix istr 3)) }
| ['0'-'9']+"I8" as istr
| "0x"['0'-'9' 'A'-'F' 'a'-'f']+"I8" as istr
    { LITERALI8 (curr_position lexbuf,
                 Char.chr (Int32.to_int (Int32.of_string
                                           (remove_suffix istr 3)))) }
| ['0'-'9']+"U8" as istr
| "0x"['0'-'9' 'A'-'F' 'a'-'f']+"U8" as istr
    { LITERALU8 (curr_position lexbuf,
                 Char.chr (Int32.to_int (Int32.of_string
                                           (remove_suffix istr 3)))) }
| ['0'-'9']+ as istr
| "0x"['0'-'9' 'A'-'F' 'a'-'f']+ as istr
    { LITERALI32 (curr_position lexbuf, Int32.of_string istr) }

(* Floating point. *)

| ['0'-'9']+'.'(['e' 'E']['0'-'9']+)?"F64" as fstr
| ['0'-'9']*'.'['0'-'9']+(['e' 'E']['0'-'9']+)?"F64" as fstr
    { LITERALF64 (curr_position lexbuf,
                  float_of_string (remove_suffix fstr 3)) }

| ['0'-'9']+'.'(['e' 'E']['0'-'9']+)?"F32" as fstr
| ['0'-'9']*'.'['0'-'9']+(['e' 'E']['0'-'9']+)?"F32" as fstr
    { LITERALF32 (curr_position lexbuf,
                  float_of_string (remove_suffix fstr 3)) }

| ['0'-'9']+'.'(['e' 'E']['0'-'9']+)?"f" as fstr
| ['0'-'9']*'.'['0'-'9']+(['e' 'E']['0'-'9']+)?"f" as fstr
    { (* For compatibility with C. *)
      LITERALF32 (curr_position lexbuf,
                  float_of_string (remove_suffix fstr 1)) }
| ['0'-'9']+'.'(['e' 'E']['0'-'9']+)? as fstr
| ['0'-'9']*'.'['0'-'9']+(['e' 'E']['0'-'9']+)? as fstr
    { LITERALF64 (curr_position lexbuf,
                  float_of_string fstr) }

| ['A'-'Z''a'-'z''_']['A'-'Z''a'-'z''_''0'-'9']* as ident
    { IDENT (curr_position lexbuf, ident) }

(* Operators. *)
| "..." { ELLIPSIS (curr_position lexbuf) }
| "->" { RARROW (curr_position lexbuf) }
| "++" { INCREMENT (curr_position lexbuf) }
| "--" { DECREMENT (curr_position lexbuf) }
| "+=" { PLUSEQUALS (curr_position lexbuf) }
| "-=" { MINUSEQUALS (curr_position lexbuf) }
| "*=" { STAREQUALS (curr_position lexbuf) }
| "/=" { SLASHEQUALS (curr_position lexbuf) }
| "%=" { PERCENTEQUALS (curr_position lexbuf) }
| "<<=" { DBLLANGLEEQUALS (curr_position lexbuf) }
| ">>=" { DBLRANGLEEQUALS (curr_position lexbuf) }
| "&=" { AMPERSANDEQUALS (curr_position lexbuf) }
| "|=" { VBAREQUALS (curr_position lexbuf) }
| "^=" { CARATEQUALS (curr_position lexbuf) }
| '.' { DOT (curr_position lexbuf) }
| '!' { LNOT (curr_position lexbuf) }
| '~' { BNOT (curr_position lexbuf) }
| '&' { AMPERSAND (curr_position lexbuf) }
| '*' { STAR (curr_position lexbuf) }
| '/' { SLASH (curr_position lexbuf) }
| '%' { PERCENT (curr_position lexbuf) }
| '+' { PLUS (curr_position lexbuf) }
| '-' { MINUS (curr_position lexbuf) }
| "<<" { DBLLANGLE (curr_position lexbuf) }
| ">>" { DBLRANGLE (curr_position lexbuf) }
| "<=" { LEQ (curr_position lexbuf) }
| '<' { LANGLE (curr_position lexbuf) }
| ">=" { GEQ (curr_position lexbuf) }
| '>' { RANGLE (curr_position lexbuf) }
| "==" { EQUALSEQUALS (curr_position lexbuf) }
| "!=" { BANGEQUALS (curr_position lexbuf) }
| '^' { CARAT (curr_position lexbuf) }
| '|' { VBAR (curr_position lexbuf) }
| "&&" { DBLAMPERSAND (curr_position lexbuf) }
| "||" { DBLVBAR (curr_position lexbuf) }
(*| '?' { QMARK }*)
| ':' { COLON (curr_position lexbuf) }
| '=' { EQUALS (curr_position lexbuf) }
| ',' { COMMA (curr_position lexbuf) }
| '(' { LPAREN (curr_position lexbuf) }
| ')' { RPAREN (curr_position lexbuf) }
| '[' { LSQUARE (curr_position lexbuf) }
| ']' { RSQUARE (curr_position lexbuf) }
| '{' { LCURLY (curr_position lexbuf) }
| '}' { RCURLY (curr_position lexbuf) }
| ';' { SEMICOLON (curr_position lexbuf) }
| eof { EOF }
| _
    { Report.err_lexing (curr_position lexbuf) (lexeme lexbuf) }
