{
  open Defparse
  open Lexing

  exception LexError of string

  let lexerror lexbuf =
    let pos = lexeme_start_p lexbuf in
    "Error: " ^ pos.pos_fname
    ^ " (line " ^ (string_of_int pos.pos_lnum)
    ^ " column " ^ (string_of_int (pos.pos_cnum - pos.pos_bol)) ^ ")\n"
    ^ "Unexpected char: " ^ (lexeme lexbuf)
}

rule deflex = parse
| [' ' '\t']+
    { deflex lexbuf }
| '\n' { new_line lexbuf; deflex lexbuf }
| ['0'-'9']+ as i_str
    { INTEGER (lexeme_start_p lexbuf, int_of_string i_str) }
| "begin" { BEGIN (lexeme_start_p lexbuf) }
| "end" { END (lexeme_start_p lexbuf) }
| "def" { DEF (lexeme_start_p lexbuf) }
| "return" { RETURN (lexeme_start_p lexbuf) }
| ['"'][^'"']*['"'] as str { STRING (lexeme_start_p lexbuf, str) }
| ['A'-'Z''a'-'z''_']['A'-'Z''a'-'z''_''0'-'9']* as ident
    { IDENT (lexeme_start_p lexbuf, ident) }
(* Operators. *)
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
(*| '.' { DOT }*)
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
(*| ':' { COLON }*)
| '=' { EQUALS (lexeme_start_p lexbuf) }
| ',' { COMMA (lexeme_start_p lexbuf) }
| '(' { LPAREN (lexeme_start_p lexbuf) }
| ')' { RPAREN (lexeme_start_p lexbuf) }
| ';' { SEMICOLON (lexeme_start_p lexbuf) }
| eof { EOF }
| _
    { raise (LexError (lexerror lexbuf)) }

