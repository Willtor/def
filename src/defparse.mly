%{
  open Ast
  open Types
  open Lexing
%}

%token <Lexing.position * int64> LITERALI64 LITERALU64
%token <Lexing.position * int32> LITERALI32 LITERALU32
%token <Lexing.position * int32> LITERALI16 LITERALU16
%token <Lexing.position * char> LITERALI8 LITERALU8
%token <Lexing.position * bool> LITERALBOOL
%token <Lexing.position * float> LITERALF32 LITERALF64
%token <Lexing.position * string> IDENT
%token <Lexing.position * string> STRING
%token <Lexing.position> TYPE
%token <Lexing.position> EXPORT DEF VAR RETURN BEGIN END IF THEN ELSE FI
%token <Lexing.position> WHILE DO DONE

(* Operators *)
%token <Lexing.position> RARROW
%token <Lexing.position> INCREMENT DECREMENT PLUSEQUALS MINUSEQUALS
%token <Lexing.position> STAREQUALS SLASHEQUALS PERCENTEQUALS DBLLANGLEEQUALS
%token <Lexing.position> DBLRANGLEEQUALS AMPERSANDEQUALS VBAREQUALS
%token <Lexing.position> CARATEQUALS DOT LNOT BNOT AMPERSAND STAR SLASH
%token <Lexing.position> PERCENT PLUS MINUS DBLLANGLE DBLRANGLE LEQ LANGLE GEQ
%token <Lexing.position> RANGLE EQUALSEQUALS BANGEQUALS CARAT VBAR
%token <Lexing.position> DBLAMPERSAND DBLVBAR (*QMARK*) (*COLON*) EQUALS COMMA

%token <Lexing.position> LPAREN RPAREN LSQUARE RSQUARE LCURLY RCURLY
%token <Lexing.position> SEMICOLON
%token EOF

%start <stmt list> defparse

(* Operator precedence. *)
(*%left COMMA*)
%right EQUALS PLUSEQUALS MINUSEQUALS STAREQUALS SLASHEQUALS PERCENTEQUALS DBLLANGLEEQUALS DBLRANGLEEQUALS AMPERSANDEQUALS CARATEQUALS VBAREQUALS
(* Ternary conditional "a ? b : c" *)
%left DBLVBAR
%left DBLAMPERSAND
%left VBAR
%left CARAT
%left AMPERSAND
%left EQUALSEQUALS BANGEQUALS
%left LANGLE LEQ RANGLE GEQ
%left DBLLANGLE DBLRANGLE
%left PLUS MINUS
%left STAR SLASH PERCENT
%left LSQUARE
(* Need to figure out pointers. dereference, addr-of *)
%right LNOT BNOT
%nonassoc POSITIVE NEGATIVE
%nonassoc PREINCR PREDECR ADDR_OF
%left INCREMENT DECREMENT DOT

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
  let (pos, vis, name, tp) = f in
  let (epos, e) = p_n_e in
  DefFcn (pos, vis, name, tp,
          match tp with
          | VarType (_, "void") ->
             [StmtExpr (epos, e);
              ReturnVoid semi_pos]
          | _ -> [Return (epos, e)])
}
| f = fcndef b = block
    { let (pos, vis, name, tp) = f in
      let (_, stmts) = b in
      DefFcn (pos, vis, name, tp, stmts) }
| p_n_e = expr SEMICOLON { let (pos, e) = p_n_e in StmtExpr (pos, e) }
| b = block { let (pos, stmts) = b in Block (pos, stmts) }
| VAR id = IDENT tp = deftype SEMICOLON
    { let (pos, name) = id
      and _, t = tp in
      VarDecl (pos, name, t, None) }
| VAR id = IDENT tp = deftype eq = EQUALS p_n_e = expr SEMICOLON
    { let (pos, name) = id
      and (_, expr) = p_n_e
      and _, t = tp in
      let binop =
        { op_pos = eq;
          op_op = OperAssign;
          op_left = ExprVar (pos, name);
          op_right = Some expr
        }
      in
      let init = ExprBinary binop in
      VarDecl (pos, name, t, Some (eq, init)) }
| p = IF p_n_e = expr THEN slist = statementlist ec = elseclause FI
    { let (_, e) = p_n_e in IfStmt (p, e, slist, ec) }
| p = WHILE p_n_e = expr DO slist = statementlist DONE
    { let (_, e) = p_n_e in WhileLoop (p, true, e, slist) }
| p = DO slist = statementlist DONE WHILE p_n_e = expr SEMICOLON
    { let (_, e) = p_n_e in WhileLoop (p, false, e, slist) }
| p = RETURN p_n_e = expr SEMICOLON
    { let (_, e) = p_n_e in Return (p, e) }
| p = RETURN SEMICOLON { ReturnVoid p }
| TYPE id = IDENT EQUALS tp = deftype SEMICOLON
    { let pos, name = id
      and _, t = tp in
      TypeDecl (pos, name, t) }

elseclause:
| ELSE slist = statementlist { Some slist }
| { None }

fcntype_with_param_names:
| LPAREN RPAREN RARROW ret = deftype
    { let pos, rtype = ret in (pos, [], rtype) }
| LPAREN plist = parameterlist RPAREN RARROW ret = deftype
    { let pos, rtype = ret in (pos, plist, rtype) }

fcntype:
| f = fcntype_with_param_names { f }
| LPAREN plist = unnamedplist RPAREN RARROW ret = deftype
    { let pos, rtype = ret in (pos, plist, rtype) }

deftype:
| s = IDENT { let (pos, ident) = s in pos, VarType (pos, ident) }
| pos = STAR tp = deftype { let _, t = tp in (pos, PtrType (pos, t)) }
| pos = LCURLY sc = structcontents RCURLY { pos, StructType sc }
| f = fcntype
    { let pos, plist, ret = f in (pos, FcnType (plist, ret)) }

structcontents:
| sc = variabledecl { [sc] }
| sc = variabledecl COMMA sclist = structcontents { sc :: sclist }

parameterlist:
| p = variabledecl { [p] }
| p = variabledecl COMMA plist = parameterlist { p :: plist }

unnamedplist:
| dt = deftype { let pos, tp = dt in [(pos, "", tp)] }
| dt = deftype COMMA plist = unnamedplist
    { let pos, tp = dt in (pos, "", tp) :: plist }

variabledecl:
| s = IDENT tp = deftype
    { let (pos, ident) = s
      and _, t = tp
      in (pos, ident, t) }

fcndef:
| export_p = EXPORT? DEF s = IDENT ftype = fcntype_with_param_names
    {
      let vis = match export_p with
        | Some p -> VisExported p
        | None -> VisLocal
      in
      let (_, plist, ret) = ftype
      and (pos, ident) = s in
      (pos, vis, ident, FcnType (plist, ret))
    }

exprlist:
| e = expr COMMA elist = exprlist { e :: elist }
| e = expr { [e] }

expr:
| i = LITERALI64 { let (pos, n) = i in pos, ExprLit (pos, LitI64 n) }
| i = LITERALU64 { let (pos, n) = i in pos, ExprLit (pos, LitU64 n) }
| i = LITERALI32 { let (pos, n) = i in pos, ExprLit (pos, LitI32 n) }
| i = LITERALU32 { let (pos, n) = i in pos, ExprLit (pos, LitU32 n) }
| i = LITERALI16 { let (pos, n) = i in pos, ExprLit (pos, LitI16 n) }
| i = LITERALU16 { let (pos, n) = i in pos, ExprLit (pos, LitU16 n) }
| i = LITERALI8  { let (pos, n) = i in pos, ExprLit (pos, LitI8  n) }
| i = LITERALU8  { let (pos, n) = i in pos, ExprLit (pos, LitU8  n) }
| i = LITERALBOOL { let (pos, b) = i in pos, ExprLit (pos, LitBool b) }
| n = LITERALF64 { let (pos, f) = n in pos, ExprLit (pos, LitF64 f) }
| n = LITERALF32 { let (pos, f) = n in pos, ExprLit (pos, LitF32 f) }
| str = STRING { let (pos, s) = str in pos, ExprString (pos, s) }
| s = IDENT LPAREN RPAREN
    { let (pos, ident) = s in
      let fcn_call =
        { fc_pos = pos;
          fc_name = ident;
          fc_args = [] } in
      pos, ExprFcnCall fcn_call }
| s = IDENT LPAREN pos_n_exprs = exprlist RPAREN
    { let (pos, ident) = s in
      let elist = List.map (fun (_, e) -> e) pos_n_exprs in
      let fcn_call =
        { fc_pos = pos;
          fc_name = ident;
          fc_args = elist }
      in
      pos, ExprFcnCall fcn_call }
| s = IDENT
    { let (pos, ident) = s in pos, ExprVar (pos, ident) }
| pos = LPAREN p_n_e = expr RPAREN { let (_, e) = p_n_e in pos, e }
| p_n_e1 = expr LSQUARE p_n_e2 = expr RSQUARE
    { let pos1, expr1 = p_n_e1 in
      let pos2, expr2 = p_n_e2 in
      pos1, ExprIndex (pos1, expr1, pos2, expr2) }
| p_n_e = expr dpos = DOT p_n_field = IDENT
    { let pos, obj = p_n_e in
      let fpos, field = p_n_field in
      pos, ExprSelectField (fpos, dpos, obj, field) }
| p_n_e = expr p = INCREMENT
    { let (exprpos, e) = p_n_e in
      let unop =
        { op_pos = p;
          op_op = OperIncr;
          op_left = e;
          op_right = None
        }
      in
      exprpos, ExprPostUnary unop }
| p_n_e = expr p = DECREMENT
    { let (exprpos, e) = p_n_e in
      let unop =
        { op_pos = p;
          op_op = OperDecr;
          op_left = e;
          op_right = None
        }
      in
      exprpos, ExprPostUnary unop }
| p = INCREMENT p_n_e = expr %prec PREINCR
    { let (_, e) = p_n_e in
      let unop =
        { op_pos = p;
          op_op = OperIncr;
          op_left = e;
          op_right = None
        }
      in
      p, ExprPreUnary unop }
| p = DECREMENT p_n_e = expr %prec PREDECR
    { let (_, e) = p_n_e in
      let unop =
        { op_pos = p;
          op_op = OperDecr;
          op_left = e;
          op_right = None
        }
      in
      p, ExprPreUnary unop }
| p = MINUS p_n_e = expr %prec NEGATIVE
    { let (_, e) = p_n_e in
      let unop =
        { op_pos = p;
          op_op = OperMinus;
          op_left = e;
          op_right = None
        }
      in
      p, ExprPreUnary unop }
| p = PLUS p_n_e = expr %prec POSITIVE
    { let (_, e) = p_n_e in
      let unop =
        { op_pos = p;
          op_op = OperPlus;
          op_left = e;
          op_right = None
        }
      in
      p, ExprPreUnary unop }
| p = LNOT p_n_e = expr
    { let (_, e) = p_n_e in
      let unop =
        { op_pos = p;
          op_op = OperLogicalNot;
          op_left = e;
          op_right = None
        }
      in
      p, ExprPreUnary unop }
| p = BNOT p_n_e = expr
    { let (_, e) = p_n_e in
      let unop =
        { op_pos = p;
          op_op = OperBitwiseNot;
          op_left = e;
          op_right = None
        }
      in
      p, ExprPreUnary unop }
| p = AMPERSAND p_n_e = expr %prec ADDR_OF
    { let _, e = p_n_e in
      let unop =
        { op_pos = p;
          op_op = OperAddrOf;
          op_left = e;
          op_right = None
        }
      in
      p, ExprPreUnary unop }
| p_n_e1 = expr p = STAR p_n_e2 = expr
    { let (exprpos, e1) = p_n_e1 in
      let (_, e2) = p_n_e2 in
      let binop =
        { op_pos = p;
          op_op = OperMult;
          op_left = e1;
          op_right = Some e2
        }
      in
      exprpos, ExprBinary binop }
| p_n_e1 = expr p = SLASH p_n_e2 = expr
    { let (exprpos, e1) = p_n_e1 in
      let (_, e2) = p_n_e2 in
      let binop =
        { op_pos = p;
          op_op = OperDiv;
          op_left = e1;
          op_right = Some e2
        }
      in
      exprpos, ExprBinary binop }
| p_n_e1 = expr p = PERCENT p_n_e2 = expr
    { let (exprpos, e1) = p_n_e1 in
      let (_, e2) = p_n_e2 in
      let binop =
        { op_pos = p;
          op_op = OperRemainder;
          op_left = e1;
          op_right = Some e2
        }
      in
      exprpos, ExprBinary binop }
| p_n_e1 = expr p = PLUS p_n_e2 = expr
    { let (exprpos, e1) = p_n_e1 in
      let (_, e2) = p_n_e2 in
      let binop =
        { op_pos = p;
          op_op = OperPlus;
          op_left = e1;
          op_right = Some e2
        }
      in
      exprpos, ExprBinary binop }
| p_n_e1 = expr p = MINUS p_n_e2 = expr
    { let (exprpos, e1) = p_n_e1 in
      let (_, e2) = p_n_e2 in
      let binop =
        { op_pos = p;
          op_op = OperMinus;
          op_left = e1;
          op_right = Some e2
        }
      in
      exprpos, ExprBinary binop }
| p_n_e1 = expr p = DBLLANGLE p_n_e2 = expr
    { let (exprpos, e1) = p_n_e1 in
      let (_, e2) = p_n_e2 in
      let binop =
        { op_pos = p;
          op_op = OperLShift;
          op_left = e1;
          op_right = Some e2
        }
      in
      exprpos, ExprBinary binop }
| p_n_e1 = expr p = DBLRANGLE p_n_e2 = expr
    { let (exprpos, e1) = p_n_e1 in
      let (_, e2) = p_n_e2 in
      let binop =
        { op_pos = p;
          op_op = OperRShift;
          op_left = e1;
          op_right = Some e2
        }
      in
      exprpos, ExprBinary binop }
| p_n_e1 = expr p = LANGLE p_n_e2 = expr
    { let (exprpos, e1) = p_n_e1 in
      let (_, e2) = p_n_e2 in
      let binop =
        { op_pos = p;
          op_op = OperLT;
          op_left = e1;
          op_right = Some e2
        }
      in
      exprpos, ExprBinary binop }
| p_n_e1 = expr p = RANGLE p_n_e2 = expr
    { let (exprpos, e1) = p_n_e1 in
      let (_, e2) = p_n_e2 in
      let binop =
        { op_pos = p;
          op_op = OperGT;
          op_left = e1;
          op_right = Some e2
        }
      in
      exprpos, ExprBinary binop }
| p_n_e1 = expr p = LEQ p_n_e2 = expr
    { let (exprpos, e1) = p_n_e1 in
      let (_, e2) = p_n_e2 in
      let binop =
        { op_pos = p;
          op_op = OperLTE;
          op_left = e1;
          op_right = Some e2
        }
      in
      exprpos, ExprBinary binop }
| p_n_e1 = expr p = GEQ p_n_e2 = expr
    { let (exprpos, e1) = p_n_e1 in
      let (_, e2) = p_n_e2 in
      let binop =
        { op_pos = p;
          op_op = OperGTE;
          op_left = e1;
          op_right = Some e2
        }
      in
      exprpos, ExprBinary binop }
| p_n_e1 = expr p = EQUALSEQUALS p_n_e2 = expr
    { let (exprpos, e1) = p_n_e1 in
      let (_, e2) = p_n_e2 in
      let binop =
        { op_pos = p;
          op_op = OperEquals;
          op_left = e1;
          op_right = Some e2
        }
      in
      exprpos, ExprBinary binop }
| p_n_e1 = expr p = BANGEQUALS p_n_e2 = expr
    { let (exprpos, e1) = p_n_e1 in
      let (_, e2) = p_n_e2 in
      let binop =
        { op_pos = p;
          op_op = OperNEquals;
          op_left = e1;
          op_right = Some e2
        }
      in
      exprpos, ExprBinary binop }
| p_n_e1 = expr p = AMPERSAND p_n_e2 = expr
    { let (exprpos, e1) = p_n_e1 in
      let (_, e2) = p_n_e2 in
      let binop =
        { op_pos = p;
          op_op = OperBitwiseAnd;
          op_left = e1;
          op_right = Some e2
        }
      in
      exprpos, ExprBinary binop }
| p_n_e1 = expr p = CARAT p_n_e2 = expr
    { let (exprpos, e1) = p_n_e1 in
      let (_, e2) = p_n_e2 in
      let binop =
        { op_pos = p;
          op_op = OperBitwiseXor;
          op_left = e1;
          op_right = Some e2
        }
      in
      exprpos, ExprBinary binop }
| p_n_e1 = expr p = VBAR p_n_e2 = expr
    { let (exprpos, e1) = p_n_e1 in
      let (_, e2) = p_n_e2 in
      let binop =
        { op_pos = p;
          op_op = OperBitwiseOr;
          op_left = e1;
          op_right = Some e2
        }
      in
      exprpos, ExprBinary binop }
| p_n_e1 = expr p = DBLAMPERSAND p_n_e2 = expr
    { let (exprpos, e1) = p_n_e1 in
      let (_, e2) = p_n_e2 in
      let binop =
        { op_pos = p;
          op_op = OperLogicalAnd;
          op_left = e1;
          op_right = Some e2
        }
      in
      exprpos, ExprBinary binop }
| p_n_e1 = expr p = DBLVBAR p_n_e2 = expr
    { let (exprpos, e1) = p_n_e1 in
      let (_, e2) = p_n_e2 in
      let binop =
        { op_pos = p;
          op_op = OperLogicalOr;
          op_left = e1;
          op_right = Some e2
        }
      in
      exprpos, ExprBinary binop }
| p_n_e1 = expr p = EQUALS p_n_e2 = expr
    { let (exprpos, e1) = p_n_e1 in
      let (_, e2) = p_n_e2 in
      let binop =
        { op_pos = p;
          op_op = OperAssign;
          op_left = e1;
          op_right = Some e2
        }
      in
      exprpos, ExprBinary binop }
| p_n_e1 = expr p = PLUSEQUALS p_n_e2 = expr
    { let (exprpos, e1) = p_n_e1 in
      let (_, e2) = p_n_e2 in
      let binop =
        { op_pos = p;
          op_op = OperPlusAssign;
          op_left = e1;
          op_right = Some e2
        }
      in
      exprpos, ExprBinary binop }
| p_n_e1 = expr p = MINUSEQUALS p_n_e2 = expr
    { let (exprpos, e1) = p_n_e1 in
      let (_, e2) = p_n_e2 in
      let binop =
        { op_pos = p;
          op_op = OperMinusAssign;
          op_left = e1;
          op_right = Some e2
        }
      in
      exprpos, ExprBinary binop }
| p_n_e1 = expr p = STAREQUALS p_n_e2 = expr
    { let (exprpos, e1) = p_n_e1 in
      let (_, e2) = p_n_e2 in
      let binop =
        { op_pos = p;
          op_op = OperMultAssign;
          op_left = e1;
          op_right = Some e2
        }
      in
      exprpos, ExprBinary binop }
| p_n_e1 = expr p = SLASHEQUALS p_n_e2 = expr
    { let (exprpos, e1) = p_n_e1 in
      let (_, e2) = p_n_e2 in
      let binop =
        { op_pos = p;
          op_op = OperDivAssign;
          op_left = e1;
          op_right = Some e2
        }
      in
      exprpos, ExprBinary binop }
| p_n_e1 = expr p = PERCENTEQUALS p_n_e2 = expr
    { let (exprpos, e1) = p_n_e1 in
      let (_, e2) = p_n_e2 in
      let binop =
        { op_pos = p;
          op_op = OperRemAssign;
          op_left = e1;
          op_right = Some e2
        }
      in
      exprpos, ExprBinary binop }
| p_n_e1 = expr p = DBLLANGLEEQUALS p_n_e2 = expr
    { let (exprpos, e1) = p_n_e1 in
      let (_, e2) = p_n_e2 in
      let binop =
        { op_pos = p;
          op_op = OperLShiftAssign;
          op_left = e1;
          op_right = Some e2
        }
      in
      exprpos, ExprBinary binop }
| p_n_e1 = expr p = DBLRANGLEEQUALS p_n_e2 = expr
    { let (exprpos, e1) = p_n_e1 in
      let (_, e2) = p_n_e2 in
      let binop =
        { op_pos = p;
          op_op = OperRShiftAssign;
          op_left = e1;
          op_right = Some e2
        }
      in
      exprpos, ExprBinary binop }
| p_n_e1 = expr p = AMPERSANDEQUALS p_n_e2 = expr
    { let (exprpos, e1) = p_n_e1 in
      let (_, e2) = p_n_e2 in
      let binop =
        { op_pos = p;
          op_op = OperBAndAssign;
          op_left = e1;
          op_right = Some e2
        }
      in
      exprpos, ExprBinary binop }
| p_n_e1 = expr p = CARATEQUALS p_n_e2 = expr
    { let (exprpos, e1) = p_n_e1 in
      let (_, e2) = p_n_e2 in
      let binop =
        { op_pos = p;
          op_op = OperBXorAssign;
          op_left = e1;
          op_right = Some e2
        }
      in
      exprpos, ExprBinary binop }
| p_n_e1 = expr p = VBAREQUALS p_n_e2 = expr
    { let (exprpos, e1) = p_n_e1 in
      let (_, e2) = p_n_e2 in
      let binop =
        { op_pos = p;
          op_op = OperBOrAssign;
          op_left = e1;
          op_right = Some e2
        }
      in
      exprpos, ExprBinary binop }