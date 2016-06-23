%{
  open Ast
%}

%token <int> INTEGER
%token PLUS MINUS MULT DIV
%token LPAREN RPAREN
%token SEMICOLON
%token EOF

%start <stmt list> defparse

%left PLUS MINUS
%left MULT DIV
%nonassoc NEGATIVE

%%

defparse:
| stmt = statement EOF { [stmt] }
| stmt = statement rest = defparse { stmt :: rest }

statement:
| e = expr SEMICOLON { StmtExpr e }

expr:
| i = INTEGER { ExprAtom (AtomInt i) }
| LPAREN e = expr RPAREN { e }
| e1 = expr PLUS e2 = expr { ExprBinary ("+", e1, e2) }
| e1 = expr MINUS e2 = expr { ExprBinary ("-", e1, e2) }
| e1 = expr MULT e2 = expr { ExprBinary ("*", e1, e2) }
| e1 = expr DIV e2 = expr { ExprBinary ("/", e1, e2) }
| MINUS e = expr %prec NEGATIVE { ExprPreUnary ("-", e) }

