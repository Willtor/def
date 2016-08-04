%{
  open Ast
  open Lexing
%}

%token <Lexing.position * int> INTEGER
%token <Lexing.position * string> IDENT
%token <Lexing.position * string> STRING
%token <Lexing.position> DEF RETURN BEGIN END

(* Operators *)
%token <Lexing.position> RARROW
%token <Lexing.position> INCREMENT DECREMENT PLUSEQUALS MINUSEQUALS
%token <Lexing.position> STAREQUALS SLASHEQUALS PERCENTEQUALS DBLLANGLEEQUALS
%token <Lexing.position> DBLRANGLEEQUALS AMPERSANDEQUALS VBAREQUALS
%token <Lexing.position> CARATEQUALS (*DOT*) LNOT BNOT AMPERSAND STAR SLASH
%token <Lexing.position> PERCENT PLUS MINUS DBLLANGLE DBLRANGLE LEQ LANGLE GEQ
%token <Lexing.position> RANGLE EQUALSEQUALS BANGEQUALS CARAT VBAR
%token <Lexing.position> DBLAMPERSAND DBLVBAR (*QMARK*) (*COLON*) EQUALS COMMA

%token <Lexing.position> LPAREN RPAREN
%token <Lexing.position> SEMICOLON
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
(*%left COMMA*)

%%

defparse:
| stmt = statement EOF { [stmt] }
| stmt = statement rest = defparse { stmt :: rest }

statementlist:
| stmt = statement slist = statementlist { stmt :: slist }
| stmt = statement { [stmt] }

block:
| BEGIN slist = statementlist END { Block slist }

statement:
| f = fcndef EQUALS e = expr SEMICOLON { DefFcn (f, StmtExpr e) }
| f = fcndef stmt = block { DefFcn (f, stmt) }
| e = expr SEMICOLON { StmtExpr e }
| RETURN e = expr SEMICOLON { Return e }

fcntype:
| LPAREN RPAREN RARROW ret = deftype
    { ([], ret) }
| LPAREN plist = parameterlist RPAREN RARROW ret = deftype
    { (plist, ret) }

deftype:
| s = IDENT { let (pos, ident) = s in VarType (pos, ident) }

parameterlist:
| p = variabledecl { [p] }
| p = variabledecl COMMA plist = parameterlist { p :: plist }

variabledecl:
| s = IDENT t = deftype { let (pos, ident) = s in (pos, ident, t) }

fcndef:
| DEF s = IDENT ftype = fcntype
    {
      let (plist, ret) = ftype
      and (pos, ident) = s in
      NamedFunction (pos, ident, plist, ret)
    }

exprlist:
| e = expr COMMA elist = exprlist { e :: elist }
| e = expr { [e] }

expr:
| i = INTEGER { let (pos, n) = i in ExprAtom (AtomInt (pos, n)) }
| str = STRING { let (pos, s) = str in ExprString (pos, s) }
| s = IDENT LPAREN RPAREN
    { let (pos, ident) = s in ExprFcnCall (pos, ident, []) }
| s = IDENT LPAREN elist = exprlist RPAREN
    { let (pos, ident) = s in ExprFcnCall (pos, ident, elist) }
| s = IDENT
    { let (pos, ident) = s in ExprAtom (AtomVar (pos, ident)) }
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
