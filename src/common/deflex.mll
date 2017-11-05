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
  open Parsetree

  (** Escaped character was unknown. *)
  let err_bad_escaped_char pos c =
    Error.err_pos ("Escaped (backslash'd) the char, " ^ (Char.escaped c)
                   ^ ", which is not recognized")
                  pos

  (** Lexing error. *)
  let err_lexing pos character =
    Error.err_pos ("Unexpected character: " ^ character) pos

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
      | _ -> err_bad_escaped_char pos c
    else c

  let remove_quotes s = String.sub s 1 ((String.length s) - 2)

  let strify = String.make 1

  let noncode : string list ref = ref []

  let push_noncode s = noncode := s :: !noncode

  let get_token_data text lexbuf =
    let nc = !noncode in
    noncode := [];
    { td_pos     = lexeme_start_p lexbuf;
      td_text    = text;
      td_noncode = nc
    }
}

rule deflex = parse
| [' ' '\t']+ as nc
    { push_noncode nc; deflex lexbuf }
| '\n' { new_line lexbuf; push_noncode "\n"; deflex lexbuf }
| "/**"([^'*']*"*"+[^'*' '/'])*[^'*']*"*"+"/" as comment
    { proc_newlines lexbuf comment;
      push_noncode comment;
      deflex lexbuf }
| "/*"([^'*']*"*"+[^'*' '/'])*[^'*']*"*"+"/" as comment
    { proc_newlines lexbuf comment;
      push_noncode comment;
      deflex lexbuf }
| "//"[^'\n']* as comment { push_noncode comment; deflex lexbuf }
| "import" as tok { IMPORT (get_token_data tok lexbuf) }
| "type" as tok { TYPE (get_token_data tok lexbuf) }
| "typedef" as tok { TYPEDEF (get_token_data tok lexbuf) }
| "begin" as tok { BEGIN (get_token_data tok lexbuf) }
| "end" as tok { END (get_token_data tok lexbuf) }
| "export" as tok { EXPORT (get_token_data tok lexbuf) }
| "opaque" as tok { OPAQUE (get_token_data tok lexbuf) }
| "def" as tok { DEF (get_token_data tok lexbuf) }
| "decl" as tok { DECL (get_token_data tok lexbuf) }
| "var" as tok { VAR (get_token_data tok lexbuf) }
| "return" as tok { RETURN (get_token_data tok lexbuf) }
| "if" as tok { IF (get_token_data tok lexbuf) }
| "then" as tok { THEN (get_token_data tok lexbuf) }
| "elif" as tok { ELIF (get_token_data tok lexbuf) }
| "else" as tok { ELSE (get_token_data tok lexbuf) }
| "fi" as tok   { FI (get_token_data tok lexbuf) }
| "for" as tok { FOR (get_token_data tok lexbuf) }
| "parfor" as tok { PARFOR (get_token_data tok lexbuf) }
| "while" as tok { WHILE (get_token_data tok lexbuf) }
| "do" as tok { DO (get_token_data tok lexbuf) }
| "od" as tok { OD (get_token_data tok lexbuf) }
| "goto" as tok { GOTO (get_token_data tok lexbuf) }
| "break" as tok { BREAK (get_token_data tok lexbuf) }
| "continue" as tok { CONTINUE (get_token_data tok lexbuf) }
| "new" as tok { NEW (get_token_data tok lexbuf) }
| "delete" as tok { DELETE (get_token_data tok lexbuf) }
| "retire" as tok { RETIRE (get_token_data tok lexbuf) }
| "xbegin" as tok { XBEGIN (get_token_data tok lexbuf) }
| "xcommit" as tok { XCOMMIT (get_token_data tok lexbuf) }
| "nil" as tok { NIL (get_token_data tok lexbuf) }
| "volatile" as tok { VOLATILE (get_token_data tok lexbuf) }
| "atomic" as tok { ATOMIC (get_token_data tok lexbuf) }
| "spawn" as tok { SPAWN (get_token_data tok lexbuf) }
| "sync" as tok { SYNC (get_token_data tok lexbuf) }
| "true" as tok { LITERALBOOL (get_token_data tok lexbuf, true) }
| "false" as tok { LITERALBOOL (get_token_data tok lexbuf, false) }
| ['"'][^'"']*['"'] as str { STRING (get_token_data str lexbuf,
                                     remove_quotes str) }

(* Integers. *)

| "'"[^'\\' '\'']|("\\"_)"'" as cstr
    { let td = get_token_data cstr lexbuf in
      LITERALI8 (td, get_i8_of_str td.td_pos cstr) }
| ['0'-'9']+"I64" as istr
| "0x"['0'-'9' 'A'-'F' 'a'-'f']+"I64" as istr
    { LITERALI64 (get_token_data istr lexbuf,
                  Int64.of_string (remove_suffix istr 3)) }
| ['0'-'9']+"U64" as istr
| "0x"['0'-'9' 'A'-'F' 'a'-'f']+"U64" as istr
    { LITERALU64 (get_token_data istr lexbuf,
                  Int64.of_string (remove_suffix istr 3)) }
| ['0'-'9']+"I32" as istr
| "0x"['0'-'9' 'A'-'F' 'a'-'f']+"I32" as istr
    { LITERALI32 (get_token_data istr lexbuf,
                  Int32.of_string (remove_suffix istr 3)) }
| ['0'-'9']+"U32" as istr
| "0x"['0'-'9' 'A'-'F' 'a'-'f']+"U32" as istr
    { LITERALU32 (get_token_data istr lexbuf,
                  Int32.of_string (remove_suffix istr 3)) }
| ['0'-'9']+"I16" as istr
| "0x"['0'-'9' 'A'-'F' 'a'-'f']+"I16" as istr
    { LITERALI16 (get_token_data istr lexbuf,
                  Int32.of_string (remove_suffix istr 3)) }
| ['0'-'9']+"U16" as istr
| "0x"['0'-'9' 'A'-'F' 'a'-'f']+"U16" as istr
    { LITERALU16 (get_token_data istr lexbuf,
                  Int32.of_string (remove_suffix istr 3)) }
| ['0'-'9']+"I8" as istr
| "0x"['0'-'9' 'A'-'F' 'a'-'f']+"I8" as istr
    { LITERALI8 (get_token_data istr lexbuf,
                 Char.chr (Int32.to_int (Int32.of_string
                                           (remove_suffix istr 3)))) }
| ['0'-'9']+"U8" as istr
| "0x"['0'-'9' 'A'-'F' 'a'-'f']+"U8" as istr
    { LITERALU8 (get_token_data istr lexbuf,
                 Char.chr (Int32.to_int (Int32.of_string
                                           (remove_suffix istr 3)))) }
| ['0'-'9']+ as istr
| "0x"['0'-'9' 'A'-'F' 'a'-'f']+ as istr
    { LITERALI32 (get_token_data istr lexbuf, Int32.of_string istr) }

(* Floating point. *)

| ['0'-'9']+'.'?(['e' 'E']['0'-'9']+)?"F64" as fstr
| ['0'-'9']*'.'?['0'-'9']+(['e' 'E']['0'-'9']+)?"F64" as fstr
    { LITERALF64 (get_token_data fstr lexbuf,
                  float_of_string (remove_suffix fstr 3)) }

| ['0'-'9']+'.'(['e' 'E']['0'-'9']+)?"F32" as fstr
| ['0'-'9']*'.'['0'-'9']+(['e' 'E']['0'-'9']+)?"F32" as fstr
    { LITERALF32 (get_token_data fstr lexbuf,
                  float_of_string (remove_suffix fstr 3)) }

| ['0'-'9']+'.'(['e' 'E']['0'-'9']+)?"f" as fstr
| ['0'-'9']*'.'['0'-'9']+(['e' 'E']['0'-'9']+)?"f" as fstr
    { (* For compatibility with C. *)
      LITERALF32 (get_token_data fstr lexbuf,
                  float_of_string (remove_suffix fstr 1)) }
| ['0'-'9']+'.'['e' 'E']['0'-'9']+ as fstr
| ['0'-'9']*'.'['0'-'9']+(['e' 'E']['0'-'9']+)? as fstr
    { LITERALF64 (get_token_data fstr lexbuf,
                  float_of_string fstr) }

| ['A'-'Z''a'-'z''_']['A'-'Z''a'-'z''_''0'-'9']* as ident
    { IDENT (get_token_data ident lexbuf) }

(* Operators. *)
| "`" as tok { BACKTICK (get_token_data (String.make 1 tok) lexbuf) }
| "..." as tok { ELLIPSIS (get_token_data tok lexbuf) }
| "->" as tok { RARROW (get_token_data tok lexbuf) }
| "++" as tok { INCREMENT (get_token_data tok lexbuf) }
| "--" as tok { DECREMENT (get_token_data tok lexbuf) }
| "+=" as tok { PLUSEQUALS (get_token_data tok lexbuf) }
| "-=" as tok { MINUSEQUALS (get_token_data tok lexbuf) }
| "*=" as tok { STAREQUALS (get_token_data tok lexbuf) }
| "/=" as tok { SLASHEQUALS (get_token_data tok lexbuf) }
| "%=" as tok { PERCENTEQUALS (get_token_data tok lexbuf) }
| "<<=" as tok { DBLLANGLEEQUALS (get_token_data tok lexbuf) }
| ">>=" as tok { DBLRANGLEEQUALS (get_token_data tok lexbuf) }
| "&=" as tok { AMPERSANDEQUALS (get_token_data tok lexbuf) }
| "|=" as tok { VBAREQUALS (get_token_data tok lexbuf) }
| "^=" as tok { CARATEQUALS (get_token_data tok lexbuf) }
| '.' as tok { DOT (get_token_data (strify tok) lexbuf) }
| '!' as tok { LNOT (get_token_data (strify tok) lexbuf) }
| '~' as tok { BNOT (get_token_data (strify tok) lexbuf) }
| '&' as tok { AMPERSAND (get_token_data (strify tok) lexbuf) }
| '*' as tok { STAR (get_token_data (strify tok) lexbuf) }
| '/' as tok { SLASH (get_token_data (strify tok) lexbuf) }
| '%' as tok { PERCENT (get_token_data (strify tok) lexbuf) }
| '+' as tok { PLUS (get_token_data (strify tok) lexbuf) }
| '-' as tok { MINUS (get_token_data (strify tok) lexbuf) }
| "<<" as tok { DBLLANGLE (get_token_data tok lexbuf) }
| ">>" as tok { DBLRANGLE (get_token_data tok lexbuf) }
| "<=" as tok { LEQ (get_token_data tok lexbuf) }
| '<' as tok { LANGLE (get_token_data (strify tok) lexbuf) }
| ">=" as tok { GEQ (get_token_data tok lexbuf) }
| '>' as tok { RANGLE (get_token_data (strify tok) lexbuf) }
| "==" as tok { EQUALSEQUALS (get_token_data tok lexbuf) }
| "!=" as tok { BANGEQUALS (get_token_data tok lexbuf) }
| '^' as tok { CARAT (get_token_data (strify tok) lexbuf) }
| '|' as tok { VBAR (get_token_data (strify tok) lexbuf) }
| "&&" as tok { DBLAMPERSAND (get_token_data tok lexbuf) }
| "||" as tok { DBLVBAR (get_token_data tok lexbuf) }
(*| '?' { QMARK }*)
| ':' as tok { COLON (get_token_data (strify tok) lexbuf) }
| '=' as tok { EQUALS (get_token_data (strify tok) lexbuf) }
| ',' as tok { COMMA (get_token_data (strify tok) lexbuf) }
| '(' as tok { LPAREN (get_token_data (strify tok) lexbuf) }
| ')' as tok { RPAREN (get_token_data (strify tok) lexbuf) }
| '[' as tok { LSQUARE (get_token_data (strify tok) lexbuf) }
| ']' as tok { RSQUARE (get_token_data (strify tok) lexbuf) }
| '{' as tok { LCURLY (get_token_data (strify tok) lexbuf) }
| '}' as tok { RCURLY (get_token_data (strify tok) lexbuf) }
| ';' as tok { SEMICOLON (get_token_data (strify tok) lexbuf) }
| eof { EOF }
| _
    { err_lexing (lexeme_start_p lexbuf) (lexeme lexbuf) }
