(* Copyright (C) 2017  DEFC Authors

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License
   as published by the Free Software Foundation; either version 2
   of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
   02110-1301, USA.
 *)

%{
  open Ast
  open Types
  open Lexing
  open Parsetree

  let rec get_documentation = function
    | [] -> None
    | [ str ] ->
       begin try let sub = String.sub str 0 3 in
                 if sub = "/**" then Some str else None
             with _ -> None
       end
    | _ :: rest -> get_documentation rest

  let string_list_of =
    List.map (fun td -> td.td_pos, td.td_text)
%}

%token <Parsetree.tokendata * int64> LITERALI64 LITERALU64
%token <Parsetree.tokendata * int32> LITERALI32 LITERALU32 LITERALI16 LITERALU16
%token <Parsetree.tokendata * char> LITERALI8 LITERALU8
%token <Parsetree.tokendata * bool> LITERALBOOL
%token <Parsetree.tokendata * float> LITERALF32 LITERALF64
%token <Parsetree.tokendata> IDENT
%token <Parsetree.tokendata * string> STRING
%token <Parsetree.tokendata> TYPE TYPEDEF
%token <Parsetree.tokendata> OPAQUE DEF DECL VAR RETURN BEGIN END IF THEN
%token <Parsetree.tokendata> ELIF ELSE FI FOR WHILE DO DONE CAST AS GOTO BREAK
%token <Parsetree.tokendata> CONTINUE NEW DELETE RETIRE NIL VOLATILE

%token <Parsetree.tokendata> EXPORT

(* Operators *)
%token <Parsetree.tokendata> ELLIPSIS RARROW
%token <Parsetree.tokendata> INCREMENT DECREMENT PLUSEQUALS MINUSEQUALS
%token <Parsetree.tokendata> STAREQUALS SLASHEQUALS PERCENTEQUALS DBLLANGLEEQUALS
%token <Parsetree.tokendata> DBLRANGLEEQUALS AMPERSANDEQUALS VBAREQUALS
%token <Parsetree.tokendata> CARATEQUALS DOT LNOT BNOT AMPERSAND STAR SLASH
%token <Parsetree.tokendata> PERCENT PLUS MINUS DBLLANGLE DBLRANGLE LEQ LANGLE GEQ
%token <Parsetree.tokendata> RANGLE EQUALSEQUALS BANGEQUALS CARAT VBAR
%token <Parsetree.tokendata> DBLAMPERSAND DBLVBAR (*QMARK*) COLON EQUALS COMMA

%token <Parsetree.tokendata> LPAREN RPAREN LSQUARE RSQUARE LCURLY RCURLY
%token <Parsetree.tokendata> SEMICOLON
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
%nonassoc ADDR_OF
%left LSQUARE
(* Need to figure out pointers. dereference, addr-of *)
%right LNOT BNOT
%nonassoc POSITIVE NEGATIVE
%nonassoc PREINCR PREDECR
%left INCREMENT DECREMENT DOT

%%

defparse:
| stmts = statement+ EOF { stmts }

block:
| p = BEGIN slist = statement+ END { (p.td_pos, slist) }

for_init:
| p = VAR id = IDENT tp = deftype EQUALS p_n_e = expr
    { let _, t = tp in
      VarDecl ([(p.td_pos, id.td_text, Some p_n_e)], t)
    }
| p_n_e = expr
    { let pos, expr = p_n_e in
      StmtExpr (pos, expr)
    }

statement:
| f = fcndef EQUALS p_n_e = expr semi = SEMICOLON {
  (* If it's an expression, it may need to be returned.  Check to see
     whether the return type is void.  If not, return it. *)
  let (pos, doc, vis, name, tp) = f in
  let (epos, e) = p_n_e in
  DefFcn (pos, doc, vis, name, tp,
          match tp with
          | VarType (_, "void", []) ->
             [StmtExpr (epos, e);
              ReturnVoid semi.td_pos]
          | _ -> [Return (epos, e)])
}
| f = fcndecl
    { let pos, vis, name, tp = f in
      DeclFcn (pos, vis, name, tp)
    }
| f = fcndef b = block
    { let (pos, doc, vis, name, tp) = f in
      let (_, stmts) = b in
      DefFcn (pos, doc, vis, name, tp, stmts) }
| p_n_e = expr SEMICOLON { let (pos, e) = p_n_e in StmtExpr (pos, e) }
| b = block { let (pos, stmts) = b in Block (pos, stmts) }
| VAR ids = idlist tp = deftype SEMICOLON
    { let _, t = tp in
      let vars = List.map (fun (p, n) -> (p, n, None)) ids in
      VarDecl (vars, t)
    }
| VAR ids = idlist tp = deftype eq = EQUALS elist = exprlist SEMICOLON
    { try
        let vars = List.map2
          (fun (p, n) e -> (p, n, Some e)) ids elist
        and _, t = tp
        in
        VarDecl (vars, t)
      with _ -> Report.err_var_decl_list_length_mismatch eq.td_pos
        (List.length ids) (List.length elist);
    }
| var = VAR LCURLY sc = structcontents RCURLY EQUALS p_n_e = expr SEMICOLON
    { InlineStructVarDecl (var.td_pos, sc, p_n_e) }
| del = DELETE p_n_e = expr SEMICOLON
    { let _, e = p_n_e in
      let expr =
        ExprFcnCall { fc_pos = del.td_pos;
                      fc_name = "forkscan_free";
                      fc_args = [ e ]
                    }
      in
      StmtExpr (del.td_pos, expr)
    }
| ret = RETIRE p_n_e = expr SEMICOLON
    { let _, e = p_n_e in
      let expr =
        ExprFcnCall { fc_pos = ret.td_pos;
                      fc_name = "forkscan_retire";
                      fc_args = [ e ]
                    }
      in
      StmtExpr (ret.td_pos, expr)
    }
| p = IF p_n_e = expr THEN slist = statement+ ec = elseclause FI
    { let (_, e) = p_n_e in IfStmt (p.td_pos, e, slist, ec) }
| p = FOR
    init = for_init? SEMICOLON
    cond = expr SEMICOLON
    iter = expr? DO
    body = statement+ DONE
    { ForLoop (p.td_pos, init, cond, iter, body) }
| p = WHILE p_n_e = expr DO slist = statement* DONE
    { let (_, e) = p_n_e in WhileLoop (p.td_pos, true, e, slist) }
| p = DO slist = statement+ DONE WHILE p_n_e = expr SEMICOLON
    { let (_, e) = p_n_e in WhileLoop (p.td_pos, false, e, slist) }
| p = RETURN p_n_e = expr SEMICOLON
    { let (_, e) = p_n_e in Return (p.td_pos, e) }
| p = RETURN SEMICOLON { ReturnVoid p.td_pos }
| vis_n_opa = pair(EXPORT, option(OPAQUE))? TYPEDEF id = IDENT EQUALS
    tp = deftype SEMICOLON
    { let _, t = tp
      and vis, opacity = match vis_n_opa with
        | None -> VisLocal, false
        | Some (expos, None) -> VisExported expos.td_pos, false
        | Some (expos, Some _) -> VisExported expos.td_pos, true
      in
      TypeDecl (id.td_pos, id.td_text, t, vis, opacity) }
| gt = GOTO id = IDENT SEMICOLON
    { Goto (gt.td_pos, id.td_text) }
| p = BREAK SEMICOLON
    { Break p.td_pos }
| id = IDENT COLON
    { Label (id.td_pos, id.td_text) }
| p = CONTINUE SEMICOLON
    { Continue p.td_pos }

elseclause:
| p = ELIF p_n_e = expr THEN slist = statement+ ec = elseclause
    { let _, e = p_n_e in Some [IfStmt (p.td_pos, e, slist, ec)] }
| ELSE slist = statement+ { Some slist }
| { None }

param:
| var = variabledecl { var }
| tp = deftype { let p, t = tp in p, "", t }
| p = ELLIPSIS { p.td_pos, "", Ellipsis p.td_pos }

paramlist:
| plist = separated_list(COMMA, param) { plist }

fcntype:
| LPAREN plist = paramlist RPAREN RARROW ret = deftype
    { let pos, rtype = ret in (pos, plist, rtype) }

deftype:
| VOLATILE s = IDENT
    { s.td_pos, VarType (s.td_pos, s.td_text, [ Volatile ]) }
| s = IDENT
    { s.td_pos, VarType (s.td_pos, s.td_text, []) }
| p = STAR tp = deftype { let _, t = tp in (p.td_pos, PtrType (p.td_pos, t)) }
| p = LSQUARE p_n_e = expr RSQUARE tp = deftype
    { let _, e = p_n_e in
      let _, t = tp in
      p.td_pos, ArrayType (p.td_pos, e, t)
    }
| p = LCURLY sc = structcontents RCURLY { p.td_pos, StructType sc }
| p = LCURLY ac = unnamedplist RCURLY { p.td_pos, StructType ac }
| f = fcntype
    { let pos, plist, ret = f in (pos, FcnType (plist, ret)) }

structcontents:
| vars = separated_nonempty_list(COMMA, variabledecl) { vars }

idlist:
| ids = separated_nonempty_list(COMMA, IDENT) { string_list_of ids }

unnamedplist:
| dtypes = separated_nonempty_list(COMMA, deftype)
    { List.map (fun (pos, tp) -> pos, "", tp) dtypes }

variabledecl:
| s = IDENT tp = deftype
    { let _, t = tp
      in (s.td_pos, s.td_text, t) }

fcndecl:
| DECL s = IDENT ftype = fcntype SEMICOLON
    { let _, plist, ret = ftype in
      s.td_pos, VisExported s.td_pos, s.td_text, FcnType (plist, ret)
    }

fcndef:
| export_p = EXPORT? DEF s = IDENT ftype = fcntype
    { let vis, doc = match export_p with
        | Some td -> VisExported td.td_pos, get_documentation td.td_noncode
        | None -> VisLocal, None
      in
      let _, plist, ret = ftype in
      s.td_pos, doc, vis, s.td_text, FcnType (plist, ret)
    }

exprlist:
| elist = separated_nonempty_list(COMMA, expr) { elist }

field_init:
| field = IDENT COLON p_n_e = expr
    { let ep, e = p_n_e in
      field.td_pos, field.td_text, ep, e
    }

struct_init:
| LCURLY flist = separated_nonempty_list(COMMA, field_init) RCURLY
    { flist }

expr:
| p = NEW tp = deftype init = struct_init?
    { let _, t = tp in
      let initlist = match init with
        | None -> []
        | Some lst -> lst
      in p.td_pos, ExprNew (p.td_pos, t, initlist)
    }
| p = NIL { p.td_pos, ExprNil p.td_pos }
| p = CAST p_n_e = expr AS tp = deftype
    { let _, e = p_n_e
      and _, t = tp in
      p.td_pos, ExprCast (p.td_pos, t, e)
    }
| p = TYPE tp = deftype
    { let _, t = tp in
      p.td_pos, ExprType (p.td_pos, t)
    }
| i = LITERALI64 { let p, n = i in p.td_pos, ExprLit (p.td_pos, LitI64 n) }
| i = LITERALU64 { let p, n = i in p.td_pos, ExprLit (p.td_pos, LitU64 n) }
| i = LITERALI32 { let p, n = i in p.td_pos, ExprLit (p.td_pos, LitI32 n) }
| i = LITERALU32 { let p, n = i in p.td_pos, ExprLit (p.td_pos, LitU32 n) }
| i = LITERALI16 { let p, n = i in p.td_pos, ExprLit (p.td_pos, LitI16 n) }
| i = LITERALU16 { let p, n = i in p.td_pos, ExprLit (p.td_pos, LitU16 n) }
| i = LITERALI8  { let p, n = i in p.td_pos, ExprLit (p.td_pos, LitI8  n) }
| i = LITERALU8  { let p, n = i in p.td_pos, ExprLit (p.td_pos, LitU8  n) }
| i = LITERALBOOL { let p, b = i in p.td_pos, ExprLit (p.td_pos, LitBool b) }
| n = LITERALF64 { let p, f = n in p.td_pos, ExprLit (p.td_pos, LitF64 f) }
| n = LITERALF32 { let p, f = n in p.td_pos, ExprLit (p.td_pos, LitF32 f) }
| str = STRING { let p, s = str in p.td_pos, ExprString (p.td_pos, s) }
| s = IDENT LPAREN RPAREN
    { let fcn_call =
        { fc_pos = s.td_pos;
          fc_name = s.td_text;
          fc_args = [] } in
      s.td_pos, ExprFcnCall fcn_call }
| s = IDENT LPAREN pos_n_exprs = exprlist RPAREN
    { let elist = List.map (fun (_, e) -> e) pos_n_exprs in
      let fcn_call =
        { fc_pos = s.td_pos;
          fc_name = s.td_text;
          fc_args = elist }
      in
      s.td_pos, ExprFcnCall fcn_call }
| s = IDENT { s.td_pos, ExprVar (s.td_pos, s.td_text) }
| p = LPAREN p_n_e = expr RPAREN { let _, e = p_n_e in p.td_pos, e }
| p = LCURLY elist = exprlist RCURLY
    { p.td_pos, ExprStaticStruct (p.td_pos, elist) }
| p_n_e1 = expr LSQUARE p_n_e2 = expr RSQUARE
    { let pos1, expr1 = p_n_e1 in
      let pos2, expr2 = p_n_e2 in
      pos1, ExprIndex (pos1, expr1, pos2, expr2) }
| p_n_e = expr dpos = DOT field = IDENT
    { let pos, obj = p_n_e in
      pos, ExprSelectField (field.td_pos, dpos.td_pos, obj,
                            FieldName field.td_text) }
| p_n_e = expr p = INCREMENT
    { let exprpos, e = p_n_e in
      let unop =
        { op_pos = p.td_pos;
          op_op = OperIncr;
          op_left = e;
          op_right = None
        }
      in
      exprpos, ExprPostUnary unop }
| p_n_e = expr p = DECREMENT
    { let exprpos, e = p_n_e in
      let unop =
        { op_pos = p.td_pos;
          op_op = OperDecr;
          op_left = e;
          op_right = None
        }
      in
      exprpos, ExprPostUnary unop }
| p = INCREMENT p_n_e = expr %prec PREINCR
    { let _, e = p_n_e in
      let unop =
        { op_pos = p.td_pos;
          op_op = OperIncr;
          op_left = e;
          op_right = None
        }
      in
      p.td_pos, ExprPreUnary unop }
| p = DECREMENT p_n_e = expr %prec PREDECR
    { let _, e = p_n_e in
      let unop =
        { op_pos = p.td_pos;
          op_op = OperDecr;
          op_left = e;
          op_right = None
        }
      in
      p.td_pos, ExprPreUnary unop }
| p = MINUS p_n_e = expr %prec NEGATIVE
    { let _, e = p_n_e in
      let unop =
        { op_pos = p.td_pos;
          op_op = OperMinus;
          op_left = e;
          op_right = None
        }
      in
      p.td_pos, ExprPreUnary unop }
| p = PLUS p_n_e = expr %prec POSITIVE
    { let _, e = p_n_e in
      let unop =
        { op_pos = p.td_pos;
          op_op = OperPlus;
          op_left = e;
          op_right = None
        }
      in
      p.td_pos, ExprPreUnary unop }
| p = LNOT p_n_e = expr
    { let _, e = p_n_e in
      let unop =
        { op_pos = p.td_pos;
          op_op = OperLogicalNot;
          op_left = e;
          op_right = None
        }
      in
      p.td_pos, ExprPreUnary unop }
| p = BNOT p_n_e = expr
    { let _, e = p_n_e in
      let unop =
        { op_pos = p.td_pos;
          op_op = OperBitwiseNot;
          op_left = e;
          op_right = None
        }
      in
      p.td_pos, ExprPreUnary unop }
| p = AMPERSAND p_n_e = expr %prec ADDR_OF
    { let _, e = p_n_e in
      let unop =
        { op_pos = p.td_pos;
          op_op = OperAddrOf;
          op_left = e;
          op_right = None
        }
      in
      p.td_pos, ExprPreUnary unop }
| p_n_e1 = expr p = STAR p_n_e2 = expr
    { let (exprpos, e1) = p_n_e1 in
      let (_, e2) = p_n_e2 in
      let binop =
        { op_pos = p.td_pos;
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
        { op_pos = p.td_pos;
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
        { op_pos = p.td_pos;
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
        { op_pos = p.td_pos;
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
        { op_pos = p.td_pos;
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
        { op_pos = p.td_pos;
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
        { op_pos = p.td_pos;
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
        { op_pos = p.td_pos;
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
        { op_pos = p.td_pos;
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
        { op_pos = p.td_pos;
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
        { op_pos = p.td_pos;
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
        { op_pos = p.td_pos;
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
        { op_pos = p.td_pos;
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
        { op_pos = p.td_pos;
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
        { op_pos = p.td_pos;
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
        { op_pos = p.td_pos;
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
        { op_pos = p.td_pos;
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
        { op_pos = p.td_pos;
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
        { op_pos = p.td_pos;
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
        { op_pos = p.td_pos;
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
        { op_pos = p.td_pos;
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
        { op_pos = p.td_pos;
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
        { op_pos = p.td_pos;
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
        { op_pos = p.td_pos;
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
        { op_pos = p.td_pos;
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
        { op_pos = p.td_pos;
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
        { op_pos = p.td_pos;
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
        { op_pos = p.td_pos;
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
        { op_pos = p.td_pos;
          op_op = OperBOrAssign;
          op_left = e1;
          op_right = Some e2
        }
      in
      exprpos, ExprBinary binop }
