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
  open Lexing
  open Operator
  open Parsetree
%}

(* Special token for parsing statements within an ISM block. *)
%token ISM_STATEMENTS

(* Special token for parsing an expression within an ISM block. *)
%token ISM_EXPRESSION

%token <Parsetree.tokendata * int64> LITERALI64 LITERALU64
%token <Parsetree.tokendata * int32> LITERALI32 LITERALU32
%token <Parsetree.tokendata * int32> LITERALI16 LITERALU16
%token <Parsetree.tokendata * char> LITERALI8 LITERALU8
%token <Parsetree.tokendata * bool> LITERALBOOL
%token <Parsetree.tokendata * float> LITERALF32 LITERALF64
%token <Parsetree.tokendata> IDENT
%token <Parsetree.tokendata * string> STRING
%token <Parsetree.tokendata> IMPORT CAST TYPE TYPEDEF ENUM PACKED
%token <Parsetree.tokendata> OPAQUE DEF DECL VAR GLOBAL RETURN BEGIN END
%token <Parsetree.tokendata> IF THEN ELIF ELSE FI
%token <Parsetree.tokendata> FOR PARFOR WHILE DO OD SWITCH
%token <Parsetree.tokendata> WITH XCASE OCASE ESAC GOTO BREAK
%token <Parsetree.tokendata> CONTINUE NEW DELETE RETIRE XFAIL OFAIL
%token <Parsetree.tokendata> NIL VOLATILE ATOMIC SPAWN SYNC WILDCARD

%token <Parsetree.tokendata> EXPORT

(* Operators *)
%token <Parsetree.tokendata> ELLIPSIS RARROW
%token <Parsetree.tokendata> INCREMENT DECREMENT PLUSEQUALS MINUSEQUALS
%token <Parsetree.tokendata> STAREQUALS SLASHEQUALS PERCENTEQUALS
%token <Parsetree.tokendata> DBLLANGLEEQUALS DBLRANGLEEQUALS AMPERSANDEQUALS
%token <Parsetree.tokendata> VBAREQUALS CARATEQUALS DOT LNOT BNOT AMPERSAND
%token <Parsetree.tokendata> STAR SLASH PERCENT PLUS MINUS DBLLANGLE DBLRANGLE
%token <Parsetree.tokendata> LEQ LANGLE GEQ RANGLE EQUALSEQUALS BANGEQUALS
%token <Parsetree.tokendata> CARAT VBAR
%token <Parsetree.tokendata> DBLAMPERSAND DBLVBAR QMARK COLON EQUALS COMMA

%token <Parsetree.tokendata> LPAREN RPAREN LSQUARE RSQUARE LCURLY RCURLY
%token <Parsetree.tokendata> SEMICOLON
%token EOF

(* ISM stand-ins. *)
%token <Parsetree.tokendata * Parsetree.ism> ISM_EXPR
%token <Parsetree.pt_stmt list> ISM_STMTS
%token <Parsetree.tokendata * Parsetree.ism> ISM_DELAYED_STMTS
%token <Parsetree.tokendata * Parsetree.ism> ISM_IDENT

%start <Parsetree.pt_stmt list> defparse

(* Operator precedence. *)
(*%left COMMA*)
%right ATOMIC
%right EQUALS PLUSEQUALS MINUSEQUALS STAREQUALS SLASHEQUALS PERCENTEQUALS DBLLANGLEEQUALS DBLRANGLEEQUALS AMPERSANDEQUALS CARATEQUALS VBAREQUALS
%nonassoc ELLIPSIS
%right QMARK (* Ternary conditional "a ? b : c" *)
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
| ISM_STATEMENTS statement+ RSQUARE { [PTS_ISM_Stmts $2] }
| ISM_EXPRESSION expr RSQUARE { [PTS_ISM_Expr $2] }

block:
| BEGIN statement* END { PTS_Begin ($1, $2, $3) }

ident:
| IDENT { IdentTok $1 }
| ISM_IDENT { let tok, ism = $1 in IdentIsm (tok.td_pos, ism) }

statement:
| ISM_STMTS { PTS_ISM_Stmts $1 }
| ISM_DELAYED_STMTS { let p, ism = $1 in PTS_ISM_DelayedStmts (p, ism) }
| IMPORT STRING SEMICOLON { PTS_Import ($1, $2, $3) }
| fcndef EQUALS expr SEMICOLON { PTS_FcnDefExpr ($1, $2, $3, $4) }
| fcndef block { PTS_FcnDefBlock ($1, $2) }
| DECL IDENT fcntype SEMICOLON { PTS_FcnDecl ($1, $2, $3, $4) }
| expr SEMICOLON { PTS_Expr ($1, $2) }
| block { $1 }
| VAR separated_nonempty_list(COMMA, IDENT) deftype SEMICOLON
    { PTS_Var ($1, $2, $3, $4) }
| GLOBAL separated_nonempty_list(COMMA, IDENT) deftype SEMICOLON
    { PTS_Var ($1, $2, $3, $4) }
| VAR separated_nonempty_list(COMMA, IDENT) deftype? EQUALS exprlist SEMICOLON
    { PTS_VarInit ($1, $2, $3, $4, $5, $6) }
| GLOBAL separated_nonempty_list(COMMA, IDENT) deftype?
    EQUALS exprlist SEMICOLON
    { PTS_VarInit ($1, $2, $3, $4, $5, $6) }
| VAR LCURLY separated_nonempty_list(COMMA, variabledecl) RCURLY
    EQUALS expr SEMICOLON
    { PTS_VarInlineStruct ($1, $2, $3, $4, $5, $6, $7) }
| GLOBAL LCURLY separated_nonempty_list(COMMA, variabledecl) RCURLY
    EQUALS expr SEMICOLON
    { PTS_VarInlineStruct ($1, $2, $3, $4, $5, $6, $7) }
| VAR LCURLY separated_nonempty_list(COMMA, IDENT) RCURLY
    EQUALS expr SEMICOLON
    { PTS_VarInlineStructInferred ($1, $2, $3, $4, $5, $6, $7) }
| GLOBAL LCURLY separated_nonempty_list(COMMA, IDENT) RCURLY
    EQUALS expr SEMICOLON
    { PTS_VarInlineStructInferred ($1, $2, $3, $4, $5, $6, $7) }
| DELETE expr SEMICOLON { PTS_DeleteExpr ($1, $2, $3) }
| RETIRE expr SEMICOLON { PTS_RetireExpr ($1, $2, $3) }
| ATOMIC BEGIN statement* fail_clause* END
    { PTS_Transaction ($1, $2, $3, $4, $5) }
| IF expr THEN statement* elifclause* elseclause? FI
    { PTS_IfStmt ($1, $2, $3, $4, $5, $6, $7) }
| FOR for_init? SEMICOLON expr SEMICOLON expr? DO statement* OD
    { PTS_ForLoop ($1, $2, $3, $4, $5, $6, $7, $8, $9) }
| PARFOR for_init? SEMICOLON expr SEMICOLON expr? DO statement* OD
    { PTS_ParforLoop ($1, $2, $3, $4, $5, $6, $7, $8, $9) }
| WHILE expr DO statement* OD { PTS_WhileLoop ($1, $2, $3, $4, $5) }
| DO statement* OD WHILE expr SEMICOLON
    { PTS_DoWhileLoop ($1, $2, $3, $4, $5, $6) }
| SWITCH expr WITH case+ ESAC { PTS_SwitchStmt ($1, $2, $3, $4, $5) }
| RETURN expr SEMICOLON { PTS_ReturnExpr ($1, $2, $3) }
| RETURN SEMICOLON { PTS_Return ($1, $2) }
| pair(EXPORT, option(OPAQUE))? TYPEDEF ident pair(EQUALS, deftype)? SEMICOLON
    { PTS_Type ($1, $2, $3, $4, $5) }
| GOTO IDENT SEMICOLON { PTS_Goto ($1, $2, $3) }
| BREAK SEMICOLON { PTS_Break ($1, $2) }
| IDENT COLON { PTS_Label ($1, $2) }
| CONTINUE SEMICOLON { PTS_Continue ($1, $2) }
| SYNC SEMICOLON { PTS_Sync ($1, $2) }

fail_clause:
| XFAIL expr COLON statement* { $1, $2, $3, $4 }
| OFAIL expr COLON statement* { $1, $2, $3, $4 }

case:
| XCASE expr COLON statement* { PTMatchCase ($1, $2, $3, $4) }
| OCASE expr COLON statement* { PTFallCase ($1, $2, $3, $4) }

for_init:
| VAR IDENT deftype EQUALS expr
    { PTForInit_Var ($1, $2, Some $3, $4, $5) }
| VAR IDENT EQUALS expr
    { PTForInit_Var ($2, $2, None, $3, $4) }
| expr { PTForInit_Expr $1 }

elifclause:
| ELIF expr THEN statement* { $1, $2, $3, $4 }

elseclause:
| ELSE statement* { $1, $2 }

param:
| variabledecl { let id, tp = $1 in PTP_Var (id, tp) }
| deftype { PTP_Type $1 }
| ELLIPSIS { PTP_Ellipsis $1 }

paramlist:
| separated_list(COMMA, param) { $1 }

fcntype:
| LPAREN paramlist RPAREN RARROW deftype
    { PTT_Fcn ($1, $2, $3, $4, $5) }

deftype:
| VOLATILE deftype { PTT_Volatile ($1, $2) }
| ident { PTT_Name $1 }
| STAR deftype { PTT_Ptr ($1, $2) }
| LSQUARE expr? RSQUARE deftype { PTT_Array ($1, $2, $3, $4) }
| LCURLY structcontents RCURLY { PTT_Struct (None, $1, $2, $3) }
| LCURLY unnamedplist RCURLY { PTT_StructUnnamed (None, $1, $2, $3) }
| PACKED LCURLY structcontents RCURLY { PTT_Struct (Some $1, $2, $3, $4) }
| PACKED LCURLY unnamedplist RCURLY { PTT_StructUnnamed (Some $1, $2, $3, $4) }
| ENUM nonempty_list(pair(VBAR, IDENT))
    (* FIXME: shift/reduce conflict since "type enum | foo" can appear
       as part of an expression where the vbar can be interpreted as
       part of the enum or as a part of the expression.  How to disallow
       the latter when reading a "type" which can't perform that kind of
       operation?  Maybe move "type" into a separate rule that can only
       appear in certain kinds of subexpressions? *)
    { PTT_Enum ($1, List.map (fun (vbar, id) -> id) $2) }
| fcntype { $1 }

structcontents:
| separated_nonempty_list(COMMA, variabledecl) { $1 }

unnamedplist:
| separated_nonempty_list(COMMA, deftype) { $1 }

variabledecl:
| IDENT deftype { $1, $2 }

fcndef:
| EXPORT? DEF ident fcntype { $1, $2, $3, $4 }

exprlist:
| separated_nonempty_list(COMMA, expr) { $1 }

field_init:
| IDENT COLON expr
  {  { ptfi_fname = $1;
       ptfi_colon = $2;
       ptfi_expr  = $3
     }
  }

struct_init:
| LCURLY separated_nonempty_list(COMMA, field_init) RCURLY
    { $1, $2, $3 }

expr:
| ISM_EXPR { let at, sexpr = $1 in PTE_IsmExpr (at, sexpr) }
| NEW deftype struct_init? { PTE_New ($1, $2, $3) }
| NIL { PTE_Nil $1 }
| TYPE deftype { PTE_Type ($1, $2) }
| LITERALI64 { PTE_I64 $1 }
| LITERALU64 { PTE_U64 $1 }
| LITERALI32 { PTE_I32 $1 }
| LITERALU32 { PTE_U32 $1 }
| LITERALI16 { PTE_I16 $1 }
| LITERALU16 { PTE_U16 $1 }
| LITERALI8  { PTE_I8  $1 }
| LITERALU8  { PTE_U8  $1 }
| LITERALBOOL { PTE_Bool $1 }
| LITERALF64 { PTE_F64 $1 }
| LITERALF32 { PTE_F32 $1 }
| STRING { PTE_String $1 }
| WILDCARD { PTE_Wildcard $1 }
| SPAWN ident LPAREN separated_list(COMMA, expr) RPAREN
  { PTE_FcnCall
      { ptfc_spawn = Some $1;
        ptfc_name = $2;
        ptfc_lparen = $3;
        ptfc_args = $4;
        ptfc_rparen = $5
      }
  }
| ident LPAREN separated_list(COMMA, expr) RPAREN
  { PTE_FcnCall
      { ptfc_spawn = None;
        ptfc_name = $1;
        ptfc_lparen = $2;
        ptfc_args = $3;
        ptfc_rparen = $4
      }
  }
| CAST deftype LPAREN expr RPAREN { PTE_Cast ($1, $2, $3, $4, $5) }
| ident { PTE_Var $1 }
| LPAREN expr RPAREN { $2 }
| LCURLY exprlist RCURLY { PTE_StaticStruct (None, $1, $2, $3) }
| PACKED LCURLY exprlist RCURLY { PTE_StaticStruct (Some $1, $2, $3, $4) }
| LSQUARE exprlist RSQUARE { PTE_StaticArray ($1, $2, $3) }
| expr LSQUARE expr RSQUARE { PTE_Index ($1, $2, $3, $4) }
| expr DOT IDENT { PTE_SelectField ($1, $2, PT_FieldString $3) }
| expr DOT LCURLY LITERALI32 RCURLY
    { let tok, n = $4 in
      PTE_SelectField ($1, $2, PT_FieldInt ($3, tok, n, $5)) }
| expr INCREMENT { PTE_PostUni ($1, (OperIncr, $2)) }
| expr DECREMENT { PTE_PostUni ($1, (OperDecr, $2)) }
| INCREMENT expr %prec PREINCR { PTE_PreUni ((OperIncr, $1), $2) }
| DECREMENT expr %prec PREDECR { PTE_PreUni ((OperDecr, $1), $2) }
| MINUS expr %prec NEGATIVE { PTE_PreUni ((OperMinus, $1), $2) }
| PLUS expr %prec POSITIVE { PTE_PreUni ((OperPlus, $1), $2) }
| LNOT expr { PTE_PreUni ((OperLogicalNot, $1), $2) }
| BNOT expr { PTE_PreUni ((OperBitwiseNot, $1), $2) }
| AMPERSAND expr %prec ADDR_OF { PTE_PreUni ((OperAddrOf, $1), $2) }
| expr STAR expr { PTE_Bin ($1, None, (OperMult, $2), $3) }
| expr SLASH expr { PTE_Bin ($1, None, (OperDiv, $2), $3) }
| expr PERCENT expr { PTE_Bin ($1, None, (OperRemainder, $2), $3) }
| expr PLUS expr { PTE_Bin ($1, None, (OperPlus, $2), $3) }
| expr MINUS expr { PTE_Bin ($1, None, (OperMinus, $2), $3) }
| expr DBLLANGLE expr { PTE_Bin ($1, None, (OperLShift, $2), $3) }
| expr DBLRANGLE expr { PTE_Bin ($1, None, (OperRShift, $2), $3) }
| expr LANGLE expr { PTE_Bin ($1, None, (OperLT, $2), $3) }
| expr RANGLE expr { PTE_Bin ($1, None, (OperGT, $2), $3) }
| expr LEQ expr { PTE_Bin ($1, None, (OperLTE, $2), $3) }
| expr GEQ expr { PTE_Bin ($1, None, (OperGTE, $2), $3) }
| expr EQUALSEQUALS expr { PTE_Bin ($1, None, (OperEquals, $2), $3) }
| expr BANGEQUALS expr { PTE_Bin ($1, None, (OperNEquals, $2), $3) }
| expr AMPERSAND expr { PTE_Bin ($1, None, (OperBitwiseAnd, $2), $3) }
| expr CARAT expr { PTE_Bin ($1, None, (OperBitwiseXor, $2), $3) }
| expr VBAR expr { PTE_Bin ($1, None, (OperBitwiseOr, $2), $3) }
| expr DBLAMPERSAND expr { PTE_Bin ($1, None, (OperLogicalAnd, $2), $3) }
| expr DBLVBAR expr { PTE_Bin ($1, None, (OperLogicalOr, $2), $3) }
| expr QMARK expr COLON expr %prec QMARK
    { PTE_TernaryCond ($1, $2, $3, $4, $5) }
| expr ELLIPSIS expr { PTE_Bin ($1, None, (OperEllipsis, $2), $3) }
| expr EQUALS expr { PTE_Bin ($1, None, (OperAssign, $2), $3) }
| expr PLUSEQUALS expr { PTE_Bin ($1, None, (OperPlusAssign, $2), $3) }
| expr MINUSEQUALS expr { PTE_Bin ($1, None, (OperMinusAssign, $2), $3) }
| expr STAREQUALS expr { PTE_Bin ($1, None, (OperMultAssign, $2), $3) }
| expr SLASHEQUALS expr { PTE_Bin ($1, None, (OperDivAssign, $2), $3) }
| expr PERCENTEQUALS expr { PTE_Bin ($1, None, (OperRemAssign, $2), $3) }
| expr DBLLANGLEEQUALS expr { PTE_Bin ($1, None, (OperLShiftAssign, $2), $3) }
| expr DBLRANGLEEQUALS expr { PTE_Bin ($1, None, (OperRShiftAssign, $2), $3) }
| expr AMPERSANDEQUALS expr { PTE_Bin ($1, None, (OperBAndAssign, $2), $3) }
| expr CARATEQUALS expr { PTE_Bin ($1, None, (OperBXorAssign, $2), $3) }
| expr VBAREQUALS expr { PTE_Bin ($1, None, (OperBOrAssign, $2), $3) }

| expr ATOMIC PLUSEQUALS expr { PTE_Bin ($1, Some $2, (OperPlusAssign, $3), $4) }
| expr ATOMIC MINUSEQUALS expr { PTE_Bin ($1, Some $2, (OperMinusAssign, $3), $4) }
| expr ATOMIC STAREQUALS expr { PTE_Bin ($1, Some $2, (OperMultAssign, $3), $4) }
| expr ATOMIC SLASHEQUALS expr { PTE_Bin ($1, Some $2, (OperDivAssign, $3), $4) }
| expr ATOMIC PERCENTEQUALS expr { PTE_Bin ($1, Some $2, (OperRemAssign, $3), $4) }
| expr ATOMIC DBLLANGLEEQUALS expr { PTE_Bin ($1, Some $2, (OperLShiftAssign, $3), $4) }
| expr ATOMIC DBLRANGLEEQUALS expr { PTE_Bin ($1, Some $2, (OperRShiftAssign, $3), $4) }
| expr ATOMIC AMPERSANDEQUALS expr { PTE_Bin ($1, Some $2, (OperBAndAssign, $3), $4) }
| expr ATOMIC CARATEQUALS expr { PTE_Bin ($1, Some $2, (OperBXorAssign, $3), $4) }
| expr ATOMIC VBAREQUALS expr { PTE_Bin ($1, Some $2, (OperBOrAssign, $3), $4) }
