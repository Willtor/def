{
  open Defparse

  exception LexError of string
}

rule deflex = parse
| [' ' '\t' '\n']+
    { deflex lexbuf }
| ['0'-'9']+ as i_str
    { INTEGER (int_of_string i_str) }
| '+' { PLUS }
| '-' { MINUS }
| '*' { MULT }
| '/' { DIV }
| '(' { LPAREN }
| ')' { RPAREN }
| ';' { SEMICOLON }
| eof { EOF }
| _
    { raise (LexError "error in the tokenizer.") }

