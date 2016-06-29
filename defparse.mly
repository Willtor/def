%{
  open Ast
%}

%token <int> INTEGER

(* Operators *)
%token INCREMENT DECREMENT PLUSEQUALS MINUSEQUALS STAREQUALS SLASHEQUALS
%token PERCENTEQUALS DBLLANGLEEQUALS DBLRANGLEEQUALS AMPERSANDEQUALS
%token VBAREQUALS CARATEQUALS (*DOT*) LNOT BNOT AMPERSAND STAR SLASH PERCENT
%token PLUS MINUS DBLLANGLE DBLRANGLE LEQ LANGLE GEQ RANGLE
%token EQUALSEQUALS BANGEQUALS CARAT VBAR DBLAMPERSAND DBLVBAR (*QMARK*)
%token (*COLON*) EQUALS (*COMMA*)

%token LPAREN RPAREN
%token SEMICOLON
%token EOF

%start <stmt list> defparse

(* Operator precedence. *)
%left INCREMENT DECREMENT (*DOT*)
%nonassoc PREINCR PREDECR
%nonassoc POSITIVE NEGATIVE
%right LNOT BNOT
(* Need to figure out pointers. dereference, addr-of *)
%left STAR SLASH PERCENT
%left PLUS MINUS
%left DBLLANGLE DBLRANGLE
%left LANGLE LEQ RANGLE GEQ
%left EQUALSEQUALS BANGEQUALS
%left AMPERSAND
%left CARAT
%left VBAR
%left DBLAMPERSAND
%left DBLVBAR
(* Ternary conditional "a ? b : c" *)
%right EQUALS PLUSEQUALS MINUSEQUALS STAREQUALS SLASHEQUALS PERCENTEQUALS DBLLANGLEEQUALS DBLRANGLEEQUALS AMPERSANDEQUALS CARATEQUALS VBAREQUALS
(* %left COMMA *)

%%

defparse:
| stmt = statement EOF { [stmt] }
| stmt = statement rest = defparse { stmt :: rest }

statement:
| e = expr SEMICOLON { StmtExpr e }

expr:
| i = INTEGER { ExprAtom (AtomInt i) }
| LPAREN e = expr RPAREN { e }
| e = expr INCREMENT { ExprPostUnary (OperIncr, e) }
| e = expr DECREMENT { ExprPostUnary (OperDecr, e) }
| INCREMENT e = expr %prec PREINCR { ExprPreUnary (OperIncr, e) }
| DECREMENT e = expr %prec PREDECR { ExprPreUnary (OperDecr, e) }
| MINUS e = expr %prec NEGATIVE { ExprPreUnary (OperMinus, e) }
| PLUS e = expr %prec POSITIVE { ExprPreUnary (OperPlus, e) }
| LNOT e = expr { ExprPreUnary (OperLogicalNot, e) }
| BNOT e = expr { ExprPreUnary (OperBitwiseNot, e) }
| e1 = expr STAR e2 = expr { ExprBinary (OperMult, e1, e2) }
| e1 = expr SLASH e2 = expr { ExprBinary (OperDiv, e1, e2) }
| e1 = expr PERCENT e2 = expr { ExprBinary (OperRemainder, e1, e2) }
| e1 = expr PLUS e2 = expr { ExprBinary (OperPlus, e1, e2) }
| e1 = expr MINUS e2 = expr { ExprBinary (OperMinus, e1, e2) }
| e1 = expr DBLLANGLE e2 = expr { ExprBinary (OperLShift, e1, e2) }
| e1 = expr DBLRANGLE e2 = expr { ExprBinary (OperRShift, e1, e2) }
| e1 = expr LANGLE e2 = expr { ExprBinary (OperLT, e1, e2) }
| e1 = expr RANGLE e2 = expr { ExprBinary (OperGT, e1, e2) }
| e1 = expr LEQ e2 = expr { ExprBinary (OperLTE, e1, e2) }
| e1 = expr GEQ e2 = expr { ExprBinary (OperGTE, e1, e2) }
| e1 = expr EQUALSEQUALS e2 = expr { ExprBinary (OperEquals, e1, e2) }
| e1 = expr BANGEQUALS e2 = expr
    { ExprPreUnary (OperLogicalNot, (ExprBinary (OperEquals, e1, e2))) }
| e1 = expr AMPERSAND e2 = expr { ExprBinary (OperBitwiseAnd, e1, e2) }
| e1 = expr CARAT e2 = expr { ExprBinary (OperBitwiseXor, e1, e2) }
| e1 = expr VBAR e2 = expr { ExprBinary (OperBitwiseOr, e1, e2) }
| e1 = expr DBLAMPERSAND e2 = expr { ExprBinary (OperLogicalAnd, e1, e2) }
| e1 = expr DBLVBAR e2 = expr { ExprBinary (OperLogicalOr, e1, e2) }
| e1 = expr EQUALS e2 = expr { ExprBinary (OperAssign, e1, e2) }
| e1 = expr PLUSEQUALS e2 = expr { ExprBinary (OperPlusAssign, e1, e2) }
| e1 = expr MINUSEQUALS e2 = expr { ExprBinary (OperMinusAssign, e1, e2) }
| e1 = expr STAREQUALS e2 = expr { ExprBinary (OperMultAssign, e1, e2) }
| e1 = expr SLASHEQUALS e2 = expr { ExprBinary (OperDivAssign, e1, e2) }
| e1 = expr PERCENTEQUALS e2 = expr { ExprBinary (OperRemAssign, e1, e2) }
| e1 = expr DBLLANGLEEQUALS e2 = expr { ExprBinary (OperLShiftAssign, e1, e2) }
| e1 = expr DBLRANGLEEQUALS e2 = expr { ExprBinary (OperRShiftAssign, e1, e2) }
| e1 = expr AMPERSANDEQUALS e2 = expr { ExprBinary (OperBAndAssign, e1, e2) }
| e1 = expr CARATEQUALS e2 = expr { ExprBinary (OperBXorAssign, e1, e2) }
| e1 = expr VBAREQUALS e2 = expr { ExprBinary (OperBOrAssign, e1, e2) }
