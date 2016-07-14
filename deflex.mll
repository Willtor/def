{
  open Defparse

  exception LexError of string
}

rule deflex = parse
| [' ' '\t' '\n']+
    { deflex lexbuf }
| ['0'-'9']+ as i_str
    { INTEGER (int_of_string i_str) }
| "begin" { BEGIN }
| "end" { END }
| "def" { DEF }
| "return" { RETURN }
| ['"'][^'"']*['"'] as str { STRING str }
| ['A'-'Z''a'-'z''_']['A'-'Z''a'-'z''_''0'-'9']* as ident
    { IDENT ident }
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

