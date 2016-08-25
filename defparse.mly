%{
  open Ast
  open Lexing
%}

%token <Lexing.position * int> INTEGER
%token <Lexing.position * string> IDENT
%token <Lexing.position * string> STRING
%token <Lexing.position> DEF VAR RETURN BEGIN END IF THEN ELSE FI

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
| p = BEGIN slist = statementlist END { (p, slist) }

statement:
| f = fcndef EQUALS p_n_e = expr semi_pos = SEMICOLON {
  (* If it's an expression, it may need to be returned.  Check to see
     whether the return type is void.  If not, return it. *)
  let (pos, name, tp) = f in
  let (epos, e) = p_n_e in
  DefFcn (pos, name, tp,
          match tp with
          | VarType (_, "void") ->
             [StmtExpr (epos, e);
              ReturnVoid semi_pos]
          | _ -> [Return (epos, e)])
}
| f = fcndef b = block
    { let (pos, name, tp) = f in
      let (_, stmts) = b in
      DefFcn (pos, name, tp, stmts) }
| p_n_e = expr SEMICOLON { let (pos, e) = p_n_e in StmtExpr (pos, e) }
| VAR id = IDENT tp = deftype SEMICOLON
    { let (pos, name) = id in VarDecl (pos, name, tp, None) }
| VAR id = IDENT tp = deftype EQUALS e = expr SEMICOLON
    { let (pos, name) = id in VarDecl (pos, name, tp, Some e) }
| p = IF p_n_e = expr THEN slist = statementlist ec = elseclause FI
    { let (_, e) = p_n_e in IfStmt (p, e, slist, ec) }
| p = RETURN p_n_e = expr SEMICOLON
    { let (_, e) = p_n_e in Return (p, e) }
| p = RETURN SEMICOLON { ReturnVoid p }

elseclause:
| ELSE slist = statementlist { Some slist }
| { None }

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
      (pos, ident, FcnType (plist, ret))
    }

exprlist:
| e = expr COMMA elist = exprlist { e :: elist }
| e = expr { [e] }

expr:
| i = INTEGER { let (pos, n) = i in (pos, ExprAtom (AtomInt (pos, n))) }
| str = STRING { let (pos, s) = str in (pos, ExprString (pos, s)) }
| s = IDENT LPAREN RPAREN
    { let (pos, ident) = s in (pos, ExprFcnCall (pos, ident, [])) }
| s = IDENT LPAREN pos_n_exprs = exprlist RPAREN
    { let (pos, ident) = s in
      let elist = List.map (fun (_, e) -> e) pos_n_exprs
      in
      (pos, ExprFcnCall (pos, ident, elist)) }
| s = IDENT
    { let (pos, ident) = s in (pos, ExprAtom (AtomVar (pos, ident))) }
| pos = LPAREN p_n_e = expr RPAREN { let (_, e) = p_n_e in (pos, e) }
| p_n_e = expr p = INCREMENT
    { let (exprpos, e) = p_n_e in
      (exprpos, ExprPostUnary (OperIncr p, e)) }
| p_n_e = expr p = DECREMENT
    { let (exprpos, e) = p_n_e in
      (exprpos, ExprPostUnary (OperDecr p, e)) }
| p = INCREMENT p_n_e = expr %prec PREINCR
    { let (_, e) = p_n_e in (p, ExprPreUnary (OperIncr p, e)) }
| p = DECREMENT p_n_e = expr %prec PREDECR
    { let (_, e) = p_n_e in (p, ExprPreUnary (OperDecr p, e)) }
| p = MINUS p_n_e = expr %prec NEGATIVE
    { let (_, e) = p_n_e in (p, ExprPreUnary (OperMinus p, e)) }
| p = PLUS p_n_e = expr %prec POSITIVE
    { let (_, e) = p_n_e in (p, ExprPreUnary (OperPlus p, e)) }
| p = LNOT p_n_e = expr
    { let (_, e) = p_n_e in (p, ExprPreUnary (OperLogicalNot p, e)) }
| p = BNOT p_n_e = expr
    { let (_, e) = p_n_e in (p, ExprPreUnary (OperBitwiseNot p, e)) }
| p_n_e1 = expr p = STAR p_n_e2 = expr
    { let (exprpos, e1) = p_n_e1 in
      let (_, e2) = p_n_e2 in
      (exprpos, ExprBinary (OperMult p, e1, e2)) }
| p_n_e1 = expr p = SLASH p_n_e2 = expr
    { let (exprpos, e1) = p_n_e1 in
      let (_, e2) = p_n_e2 in
      (exprpos, ExprBinary (OperDiv p, e1, e2)) }
| p_n_e1 = expr p = PERCENT p_n_e2 = expr
    { let (exprpos, e1) = p_n_e1 in
      let (_, e2) = p_n_e2 in
      (exprpos, ExprBinary (OperRemainder p, e1, e2)) }
| p_n_e1 = expr p = PLUS p_n_e2 = expr
    { let (exprpos, e1) = p_n_e1 in
      let (_, e2) = p_n_e2 in
      (exprpos, ExprBinary (OperPlus p, e1, e2)) }
| p_n_e1 = expr p = MINUS p_n_e2 = expr
    { let (exprpos, e1) = p_n_e1 in
      let (_, e2) = p_n_e2 in
      (exprpos, ExprBinary (OperMinus p, e1, e2)) }
| p_n_e1 = expr p = DBLLANGLE p_n_e2 = expr
    { let (exprpos, e1) = p_n_e1 in
      let (_, e2) = p_n_e2 in
      (exprpos, ExprBinary (OperLShift p, e1, e2)) }
| p_n_e1 = expr p = DBLRANGLE p_n_e2 = expr
    { let (exprpos, e1) = p_n_e1 in
      let (_, e2) = p_n_e2 in
      (exprpos, ExprBinary (OperRShift p, e1, e2)) }
| p_n_e1 = expr p = LANGLE p_n_e2 = expr
    { let (exprpos, e1) = p_n_e1 in
      let (_, e2) = p_n_e2 in
      (exprpos, ExprBinary (OperLT p, e1, e2)) }
| p_n_e1 = expr p = RANGLE p_n_e2 = expr
    { let (exprpos, e1) = p_n_e1 in
      let (_, e2) = p_n_e2 in
      (exprpos, ExprBinary (OperGT p, e1, e2)) }
| p_n_e1 = expr p = LEQ p_n_e2 = expr
    { let (exprpos, e1) = p_n_e1 in
      let (_, e2) = p_n_e2 in
      (exprpos, ExprBinary (OperLTE p, e1, e2)) }
| p_n_e1 = expr p = GEQ p_n_e2 = expr
    { let (exprpos, e1) = p_n_e1 in
      let (_, e2) = p_n_e2 in
      (exprpos, ExprBinary (OperGTE p, e1, e2)) }
| p_n_e1 = expr p = EQUALSEQUALS p_n_e2 = expr
    { let (exprpos, e1) = p_n_e1 in
      let (_, e2) = p_n_e2 in
      (exprpos, ExprBinary (OperEquals p, e1, e2)) }
| p_n_e1 = expr p = BANGEQUALS p_n_e2 = expr
    { let (exprpos, e1) = p_n_e1 in
      let (_, e2) = p_n_e2 in
      (exprpos, ExprBinary (OperNEquals p, e1, e2)) }
| p_n_e1 = expr p = AMPERSAND p_n_e2 = expr
    { let (exprpos, e1) = p_n_e1 in
      let (_, e2) = p_n_e2 in
      (exprpos, ExprBinary (OperBitwiseAnd p, e1, e2)) }
| p_n_e1 = expr p = CARAT p_n_e2 = expr
    { let (exprpos, e1) = p_n_e1 in
      let (_, e2) = p_n_e2 in
      (exprpos, ExprBinary (OperBitwiseXor p, e1, e2)) }
| p_n_e1 = expr p = VBAR p_n_e2 = expr
    { let (exprpos, e1) = p_n_e1 in
      let (_, e2) = p_n_e2 in
      (exprpos, ExprBinary (OperBitwiseOr p, e1, e2)) }
| p_n_e1 = expr p = DBLAMPERSAND p_n_e2 = expr
    { let (exprpos, e1) = p_n_e1 in
      let (_, e2) = p_n_e2 in
      (exprpos, ExprBinary (OperLogicalAnd p, e1, e2)) }
| p_n_e1 = expr p = DBLVBAR p_n_e2 = expr
    { let (exprpos, e1) = p_n_e1 in
      let (_, e2) = p_n_e2 in
      (exprpos, ExprBinary (OperLogicalOr p, e1, e2)) }
| p_n_e1 = expr p = EQUALS p_n_e2 = expr
    { let (exprpos, e1) = p_n_e1 in
      let (_, e2) = p_n_e2 in
      (exprpos, ExprBinary (OperAssign p, e1, e2)) }
| p_n_e1 = expr p = PLUSEQUALS p_n_e2 = expr
    { let (exprpos, e1) = p_n_e1 in
      let (_, e2) = p_n_e2 in
      (exprpos, ExprBinary (OperPlusAssign p, e1, e2)) }
| p_n_e1 = expr p = MINUSEQUALS p_n_e2 = expr
    { let (exprpos, e1) = p_n_e1 in
      let (_, e2) = p_n_e2 in
      (exprpos, ExprBinary (OperMinusAssign p, e1, e2)) }
| p_n_e1 = expr p = STAREQUALS p_n_e2 = expr
    { let (exprpos, e1) = p_n_e1 in
      let (_, e2) = p_n_e2 in
      (exprpos, ExprBinary (OperMultAssign p, e1, e2)) }
| p_n_e1 = expr p = SLASHEQUALS p_n_e2 = expr
    { let (exprpos, e1) = p_n_e1 in
      let (_, e2) = p_n_e2 in
      (exprpos, ExprBinary (OperDivAssign p, e1, e2)) }
| p_n_e1 = expr p = PERCENTEQUALS p_n_e2 = expr
    { let (exprpos, e1) = p_n_e1 in
      let (_, e2) = p_n_e2 in
      (exprpos, ExprBinary (OperRemAssign p, e1, e2)) }
| p_n_e1 = expr p = DBLLANGLEEQUALS p_n_e2 = expr
    { let (exprpos, e1) = p_n_e1 in
      let (_, e2) = p_n_e2 in
      (exprpos, ExprBinary (OperLShiftAssign p, e1, e2)) }
| p_n_e1 = expr p = DBLRANGLEEQUALS p_n_e2 = expr
    { let (exprpos, e1) = p_n_e1 in
      let (_, e2) = p_n_e2 in
      (exprpos, ExprBinary (OperRShiftAssign p, e1, e2)) }
| p_n_e1 = expr p = AMPERSANDEQUALS p_n_e2 = expr
    { let (exprpos, e1) = p_n_e1 in
      let (_, e2) = p_n_e2 in
      (exprpos, ExprBinary (OperBAndAssign p, e1, e2)) }
| p_n_e1 = expr p = CARATEQUALS p_n_e2 = expr
    { let (exprpos, e1) = p_n_e1 in
      let (_, e2) = p_n_e2 in
      (exprpos, ExprBinary (OperBXorAssign p, e1, e2)) }
| p_n_e1 = expr p = VBAREQUALS p_n_e2 = expr
    { let (exprpos, e1) = p_n_e1 in
      let (_, e2) = p_n_e2 in
      (exprpos, ExprBinary (OperBOrAssign p, e1, e2)) }
