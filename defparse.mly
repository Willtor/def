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
| f = fcndef EQUALS e = expr SEMICOLON {
  (* If it's an expression, it may need to be returned.  Check to see
     whether the return type is void.  If not, return it. *)
  DefFcn (f,
          match f with
          | NamedFunction (_, _, _, VarType (_, "void")) -> StmtExpr e
          | _ -> Return e)
}
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
| e = expr p = INCREMENT { ExprPostUnary (OperIncr p, e) }
| e = expr p = DECREMENT { ExprPostUnary (OperDecr p, e) }
| p = INCREMENT e = expr %prec PREINCR { ExprPreUnary (OperIncr p, e) }
| p = DECREMENT e = expr %prec PREDECR { ExprPreUnary (OperDecr p, e) }
| p = MINUS e = expr %prec NEGATIVE { ExprPreUnary (OperMinus p, e) }
| p = PLUS e = expr %prec POSITIVE { ExprPreUnary (OperPlus p, e) }
| p = LNOT e = expr { ExprPreUnary (OperLogicalNot p, e) }
| p = BNOT e = expr { ExprPreUnary (OperBitwiseNot p, e) }
| e1 = expr p = STAR e2 = expr { ExprBinary (OperMult p, e1, e2) }
| e1 = expr p = SLASH e2 = expr { ExprBinary (OperDiv p, e1, e2) }
| e1 = expr p = PERCENT e2 = expr { ExprBinary (OperRemainder p, e1, e2) }
| e1 = expr p = PLUS e2 = expr { ExprBinary (OperPlus p, e1, e2) }
| e1 = expr p = MINUS e2 = expr { ExprBinary (OperMinus p, e1, e2) }
| e1 = expr p = DBLLANGLE e2 = expr { ExprBinary (OperLShift p, e1, e2) }
| e1 = expr p = DBLRANGLE e2 = expr { ExprBinary (OperRShift p, e1, e2) }
| e1 = expr p = LANGLE e2 = expr { ExprBinary (OperLT p, e1, e2) }
| e1 = expr p = RANGLE e2 = expr { ExprBinary (OperGT p, e1, e2) }
| e1 = expr p = LEQ e2 = expr { ExprBinary (OperLTE p, e1, e2) }
| e1 = expr p = GEQ e2 = expr { ExprBinary (OperGTE p, e1, e2) }
| e1 = expr p = EQUALSEQUALS e2 = expr { ExprBinary (OperEquals p, e1, e2) }
| e1 = expr p = BANGEQUALS e2 = expr { ExprBinary (OperNEquals p, e1, e2) }
| e1 = expr p = AMPERSAND e2 = expr { ExprBinary (OperBitwiseAnd p, e1, e2) }
| e1 = expr p = CARAT e2 = expr { ExprBinary (OperBitwiseXor p, e1, e2) }
| e1 = expr p = VBAR e2 = expr { ExprBinary (OperBitwiseOr p, e1, e2) }
| e1 = expr p = DBLAMPERSAND e2 = expr
    { ExprBinary (OperLogicalAnd p, e1, e2) }
| e1 = expr p = DBLVBAR e2 = expr { ExprBinary (OperLogicalOr p, e1, e2) }
| e1 = expr p = EQUALS e2 = expr { ExprBinary (OperAssign p, e1, e2) }
| e1 = expr p = PLUSEQUALS e2 = expr { ExprBinary (OperPlusAssign p, e1, e2) }
| e1 = expr p = MINUSEQUALS e2 = expr
    { ExprBinary (OperMinusAssign p, e1, e2) }
| e1 = expr p = STAREQUALS e2 = expr { ExprBinary (OperMultAssign p, e1, e2) }
| e1 = expr p = SLASHEQUALS e2 = expr { ExprBinary (OperDivAssign p, e1, e2) }
| e1 = expr p = PERCENTEQUALS e2 = expr
    { ExprBinary (OperRemAssign p, e1, e2) }
| e1 = expr p = DBLLANGLEEQUALS e2 = expr
    { ExprBinary (OperLShiftAssign p, e1, e2) }
| e1 = expr p = DBLRANGLEEQUALS e2 = expr
    { ExprBinary (OperRShiftAssign p, e1, e2) }
| e1 = expr p = AMPERSANDEQUALS e2 = expr
    { ExprBinary (OperBAndAssign p, e1, e2) }
| e1 = expr p = CARATEQUALS e2 = expr { ExprBinary (OperBXorAssign p, e1, e2) }
| e1 = expr p = VBAREQUALS e2 = expr { ExprBinary (OperBOrAssign p, e1, e2) }
