{
  open Defparse
  open Lexing

  exception LexError of string
}

rule deflex = parse
| [' ' '\t']+
    { deflex lexbuf }
| '\n' { new_line lexbuf; deflex lexbuf }
| ['0'-'9']+ as i_str
    { INTEGER (lexeme_start_p lexbuf, int_of_string i_str) }
| "begin" { BEGIN }
| "end" { END }
| "def" { DEF }
| "return" { RETURN }
| ['"'][^'"']*['"'] as str { STRING (lexeme_start_p lexbuf, str) }
| ['A'-'Z''a'-'z''_']['A'-'Z''a'-'z''_''0'-'9']* as ident
    { IDENT (lexeme_start_p lexbuf, ident) }
(* Operators. *)
| "->" { RARROW }
| "++" { INCREMENT }
| "--" { DECREMENT }
| "+=" { PLUSEQUALS }
| "-=" { MINUSEQUALS }
| "*=" { STAREQUALS }
| "/=" { SLASHEQUALS }
| "%=" { PERCENTEQUALS }
| "<<=" { DBLLANGLEEQUALS }
| ">>=" { DBLRANGLEEQUALS }
| "&=" { AMPERSANDEQUALS }
| "|=" { VBAREQUALS }
| "^=" { CARATEQUALS }
(*| '.' { DOT }*)
| '!' { LNOT }
| '~' { BNOT }
| '&' { AMPERSAND }
| '*' { STAR }
| '/' { SLASH }
| '%' { PERCENT }
| '+' { PLUS }
| '-' { MINUS }
| "<<" { DBLLANGLE }
| ">>" { DBLRANGLE }
| "<=" { LEQ }
| '<' { LANGLE }
| ">=" { GEQ }
| '>' { RANGLE }
| "==" { EQUALSEQUALS }
| "!=" { BANGEQUALS }
| '^' { CARAT }
| '|' { VBAR }
| "&&" { DBLAMPERSAND }
| "||" { DBLVBAR }
(*| '?' { QMARK }*)
(*| ':' { COLON }*)
| '=' { EQUALS }
| ',' { COMMA }
| '(' { LPAREN }
| ')' { RPAREN }
| ';' { SEMICOLON }
| eof { EOF }
| _
    { raise (LexError "error in the tokenizer.") }

