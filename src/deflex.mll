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
}

rule deflex = parse
| [' ' '\t']+
    { deflex lexbuf }
| '\n' { new_line lexbuf; deflex lexbuf }
| "/*"([^'*']*"*"+[^'*' '/'])*[^'*']*"*"+"/" as comment
    { proc_newlines lexbuf comment;
      deflex lexbuf }
| "//"[^'\n']* { deflex lexbuf }
| "type" { TYPE (lexeme_start_p lexbuf) }
| "typedef" { TYPEDEF (lexeme_start_p lexbuf) }
| "begin" { BEGIN (lexeme_start_p lexbuf) }
| "end" { END (lexeme_start_p lexbuf) }
| "export" { EXPORT (lexeme_start_p lexbuf) }
| "def" { DEF (lexeme_start_p lexbuf) }
| "var" { VAR (lexeme_start_p lexbuf) }
| "return" { RETURN (lexeme_start_p lexbuf) }
| "if" { IF (lexeme_start_p lexbuf) }
| "then" { THEN (lexeme_start_p lexbuf) }
| "else" { ELSE (lexeme_start_p lexbuf) }
| "fi"   { FI (lexeme_start_p lexbuf) }
| "while" { WHILE (lexeme_start_p lexbuf) }
| "do" { DO (lexeme_start_p lexbuf) }
| "done" { DONE (lexeme_start_p lexbuf) }
| "cast" { CAST (lexeme_start_p lexbuf) }
| "as" { AS (lexeme_start_p lexbuf) }
| "goto" { GOTO (lexeme_start_p lexbuf) }
| "continue" { CONTINUE (lexeme_start_p lexbuf) }
| "new" { NEW (lexeme_start_p lexbuf) }
| "delete" { DELETE (lexeme_start_p lexbuf) }
| "retire" { RETIRE (lexeme_start_p lexbuf) }
| "nil" { NIL (lexeme_start_p lexbuf) }
| "true" { LITERALBOOL (lexeme_start_p lexbuf, true) }
| "false" { LITERALBOOL (lexeme_start_p lexbuf, false) }
| ['"'][^'"']*['"'] as str { STRING (lexeme_start_p lexbuf, str) }

(* Integers. *)

| "'"[^'\\' '\'']|("\\"_)"'" as cstr
    { let pos = lexeme_start_p lexbuf in
      LITERALI8 (pos, get_i8_of_str pos cstr) }
| ['0'-'9']+"I64" as istr
| "0x"['0'-'9' 'A'-'F' 'a'-'f']+"I64" as istr
    { LITERALI64 (lexeme_start_p lexbuf,
                  Int64.of_string (remove_suffix istr 3)) }
| ['0'-'9']+"U64" as istr
| "0x"['0'-'9' 'A'-'F' 'a'-'f']+"U64" as istr
    { LITERALU64 (lexeme_start_p lexbuf,
                  Int64.of_string (remove_suffix istr 3)) }
| ['0'-'9']+"I32" as istr
| "0x"['0'-'9' 'A'-'F' 'a'-'f']+"I32" as istr
    { LITERALI32 (lexeme_start_p lexbuf,
                  Int32.of_string (remove_suffix istr 3)) }
| ['0'-'9']+"U32" as istr
| "0x"['0'-'9' 'A'-'F' 'a'-'f']+"U32" as istr
    { LITERALU32 (lexeme_start_p lexbuf,
                  Int32.of_string (remove_suffix istr 3)) }
| ['0'-'9']+"I16" as istr
| "0x"['0'-'9' 'A'-'F' 'a'-'f']+"I16" as istr
    { LITERALI16 (lexeme_start_p lexbuf,
                  Int32.of_string (remove_suffix istr 3)) }
| ['0'-'9']+"U16" as istr
| "0x"['0'-'9' 'A'-'F' 'a'-'f']+"U16" as istr
    { LITERALU16 (lexeme_start_p lexbuf,
                  Int32.of_string (remove_suffix istr 3)) }
| ['0'-'9']+"I8" as istr
| "0x"['0'-'9' 'A'-'F' 'a'-'f']+"I8" as istr
    { LITERALI8 (lexeme_start_p lexbuf,
                 Char.chr (Int32.to_int (Int32.of_string
                                           (remove_suffix istr 3)))) }
| ['0'-'9']+"U8" as istr
| "0x"['0'-'9' 'A'-'F' 'a'-'f']+"U8" as istr
    { LITERALU8 (lexeme_start_p lexbuf,
                 Char.chr (Int32.to_int (Int32.of_string
                                           (remove_suffix istr 3)))) }
| ['0'-'9']+ as istr
| "0x"['0'-'9' 'A'-'F' 'a'-'f']+ as istr
    { LITERALI32 (lexeme_start_p lexbuf, Int32.of_string istr) }

(* Floating point. *)

| ['0'-'9']+'.'(['e' 'E']['0'-'9']+)?"F64" as fstr
| ['0'-'9']*'.'['0'-'9']+(['e' 'E']['0'-'9']+)?"F64" as fstr
    { LITERALF64 (lexeme_start_p lexbuf,
                  float_of_string (remove_suffix fstr 3)) }

| ['0'-'9']+'.'(['e' 'E']['0'-'9']+)?"F32" as fstr
| ['0'-'9']*'.'['0'-'9']+(['e' 'E']['0'-'9']+)?"F32" as fstr
    { LITERALF32 (lexeme_start_p lexbuf,
                  float_of_string (remove_suffix fstr 3)) }

| ['0'-'9']+'.'(['e' 'E']['0'-'9']+)?"f" as fstr
| ['0'-'9']*'.'['0'-'9']+(['e' 'E']['0'-'9']+)?"f" as fstr
    { (* For compatibility with C. *)
      LITERALF32 (lexeme_start_p lexbuf,
                  float_of_string (remove_suffix fstr 1)) }
| ['0'-'9']+'.'(['e' 'E']['0'-'9']+)? as fstr
| ['0'-'9']*'.'['0'-'9']+(['e' 'E']['0'-'9']+)? as fstr
    { LITERALF64 (lexeme_start_p lexbuf,
                  float_of_string fstr) }

| ['A'-'Z''a'-'z''_']['A'-'Z''a'-'z''_''0'-'9']* as ident
    { IDENT (lexeme_start_p lexbuf, ident) }

(* Operators. *)
| "..." { ELLIPSIS (lexeme_start_p lexbuf) }
| "->" { RARROW (lexeme_start_p lexbuf) }
| "++" { INCREMENT (lexeme_start_p lexbuf) }
| "--" { DECREMENT (lexeme_start_p lexbuf) }
| "+=" { PLUSEQUALS (lexeme_start_p lexbuf) }
| "-=" { MINUSEQUALS (lexeme_start_p lexbuf) }
| "*=" { STAREQUALS (lexeme_start_p lexbuf) }
| "/=" { SLASHEQUALS (lexeme_start_p lexbuf) }
| "%=" { PERCENTEQUALS (lexeme_start_p lexbuf) }
| "<<=" { DBLLANGLEEQUALS (lexeme_start_p lexbuf) }
| ">>=" { DBLRANGLEEQUALS (lexeme_start_p lexbuf) }
| "&=" { AMPERSANDEQUALS (lexeme_start_p lexbuf) }
| "|=" { VBAREQUALS (lexeme_start_p lexbuf) }
| "^=" { CARATEQUALS (lexeme_start_p lexbuf) }
| '.' { DOT (lexeme_start_p lexbuf) }
| '!' { LNOT (lexeme_start_p lexbuf) }
| '~' { BNOT (lexeme_start_p lexbuf) }
| '&' { AMPERSAND (lexeme_start_p lexbuf) }
| '*' { STAR (lexeme_start_p lexbuf) }
| '/' { SLASH (lexeme_start_p lexbuf) }
| '%' { PERCENT (lexeme_start_p lexbuf) }
| '+' { PLUS (lexeme_start_p lexbuf) }
| '-' { MINUS (lexeme_start_p lexbuf) }
| "<<" { DBLLANGLE (lexeme_start_p lexbuf) }
| ">>" { DBLRANGLE (lexeme_start_p lexbuf) }
| "<=" { LEQ (lexeme_start_p lexbuf) }
| '<' { LANGLE (lexeme_start_p lexbuf) }
| ">=" { GEQ (lexeme_start_p lexbuf) }
| '>' { RANGLE (lexeme_start_p lexbuf) }
| "==" { EQUALSEQUALS (lexeme_start_p lexbuf) }
| "!=" { BANGEQUALS (lexeme_start_p lexbuf) }
| '^' { CARAT (lexeme_start_p lexbuf) }
| '|' { VBAR (lexeme_start_p lexbuf) }
| "&&" { DBLAMPERSAND (lexeme_start_p lexbuf) }
| "||" { DBLVBAR (lexeme_start_p lexbuf) }
(*| '?' { QMARK }*)
| ':' { COLON (lexeme_start_p lexbuf) }
| '=' { EQUALS (lexeme_start_p lexbuf) }
| ',' { COMMA (lexeme_start_p lexbuf) }
| '(' { LPAREN (lexeme_start_p lexbuf) }
| ')' { RPAREN (lexeme_start_p lexbuf) }
| '[' { LSQUARE (lexeme_start_p lexbuf) }
| ']' { RSQUARE (lexeme_start_p lexbuf) }
| '{' { LCURLY (lexeme_start_p lexbuf) }
| '}' { RCURLY (lexeme_start_p lexbuf) }
| ';' { SEMICOLON (lexeme_start_p lexbuf) }
| eof { EOF }
| _
    { Report.err_lexing (lexeme_start_p lexbuf) (lexeme lexbuf) }
