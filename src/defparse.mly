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
  open Types
  open Lexing
  open Parsetree
%}

%token <Parsetree.tokendata * int64> LITERALI64 LITERALU64
%token <Parsetree.tokendata * int32> LITERALI32 LITERALU32
%token <Parsetree.tokendata * int32> LITERALI16 LITERALU16
%token <Parsetree.tokendata * char> LITERALI8 LITERALU8
%token <Parsetree.tokendata * bool> LITERALBOOL
%token <Parsetree.tokendata * float> LITERALF32 LITERALF64
%token <Parsetree.tokendata> IDENT
%token <Parsetree.tokendata * string> STRING
%token <Parsetree.tokendata> IMPORT TEMPLATE TYPE TYPEDEF
%token <Parsetree.tokendata> OPAQUE DEF DECL VAR RETURN BEGIN END IF THEN
%token <Parsetree.tokendata> ELIF ELSE FI FOR WHILE DO OD CAST AS GOTO BREAK
%token <Parsetree.tokendata> CONTINUE NEW DELETE RETIRE XBEGIN XCOMMIT
%token <Parsetree.tokendata> NIL VOLATILE SPAWN SYNC

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
%token <Parsetree.tokendata> DBLAMPERSAND DBLVBAR (*QMARK*) COLON EQUALS COMMA

%token <Parsetree.tokendata> LPAREN RPAREN LSQUARE RSQUARE LCURLY RCURLY
%token <Parsetree.tokendata> SEMICOLON
%token EOF

%start <pt_stmt list> defparse

(* Operator precedence. *)
(*%left COMMA*)
%right EQUALS PLUSEQUALS MINUSEQUALS STAREQUALS SLASHEQUALS PERCENTEQUALS DBLLANGLEEQUALS DBLRANGLEEQUALS AMPERSANDEQUALS CARATEQUALS VBAREQUALS
(* Ternary conditional "a ? b : c" *)
%nonassoc ELLIPSIS
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
| BEGIN statement* END { PTS_Begin ($1, $2, $3) }

statement:
| IMPORT STRING SEMICOLON { PTS_Import ($1, $2, $3) }
| fcndef EQUALS expr SEMICOLON { PTS_FcnDefExpr ($1, $2, $3, $4) }
| fcndef block { PTS_FcnDefBlock ($1, $2) }
| DECL IDENT fcntype SEMICOLON { PTS_FcnDecl ($1, $2, $3, $4) }
| expr SEMICOLON { PTS_Expr ($1, $2) }
| block { $1 }
| VAR separated_nonempty_list(COMMA, IDENT) deftype SEMICOLON
    { PTS_Var ($1, $2, $3, $4) }
| VAR separated_nonempty_list(COMMA, IDENT) deftype EQUALS exprlist SEMICOLON
    { PTS_VarInit ($1, $2, $3, $4, $5, $6) }
| VAR LCURLY separated_nonempty_list(COMMA, variabledecl) RCURLY
    EQUALS expr SEMICOLON
    { PTS_VarInlineStruct ($1, $2, $3, $4, $5, $6, $7) }
| DELETE expr SEMICOLON { PTS_DeleteExpr ($1, $2, $3) }
| RETIRE expr SEMICOLON { PTS_RetireExpr ($1, $2, $3) }
| XBEGIN SEMICOLON { PTS_XBegin ($1, $2) }
| XCOMMIT SEMICOLON { PTS_XCommit ($1, $2) }
| IF expr THEN statement* elifclause* elseclause? FI
    { PTS_IfStmt ($1, $2, $3, $4, $5, $6, $7) }
| FOR for_init? SEMICOLON expr SEMICOLON expr? DO statement* OD
    { PTS_ForLoop ($1, $2, $3, $4, $5, $6, $7, $8, $9) }
| WHILE expr DO statement* OD { PTS_WhileLoop ($1, $2, $3, $4, $5) }
| DO statement* OD WHILE expr SEMICOLON
    { PTS_DoWhileLoop ($1, $2, $3, $4, $5, $6) }
| RETURN expr SEMICOLON { PTS_ReturnExpr ($1, $2, $3) }
| RETURN SEMICOLON { PTS_Return ($1, $2) }
| pair(EXPORT, option(OPAQUE))? TYPEDEF IDENT EQUALS deftype SEMICOLON
    { PTS_Type ($1, $2, $3, $4, $5, $6) }
| GOTO IDENT SEMICOLON { PTS_Goto ($1, $2, $3) }
| BREAK SEMICOLON { PTS_Break ($1, $2) }
| IDENT COLON { PTS_Label ($1, $2) }
| CONTINUE SEMICOLON { PTS_Continue ($1, $2) }
| SYNC SEMICOLON { PTS_Sync ($1, $2) }

for_init:
| VAR IDENT deftype EQUALS expr
    { PTForInit_Var ($1, $2, $3, $4, $5) }
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
| IDENT { PTT_Name $1 }
| STAR deftype { PTT_Ptr ($1, $2) }
| LSQUARE expr? RSQUARE deftype { PTT_Array ($1, $2, $3, $4) }
| LCURLY structcontents RCURLY { PTT_Struct ($1, $2, $3) }
| LCURLY unnamedplist RCURLY { PTT_StructUnnamed ($1, $2, $3) }
| fcntype { $1 }

structcontents:
| separated_nonempty_list(COMMA, variabledecl) { $1 }

unnamedplist:
| separated_nonempty_list(COMMA, deftype) { $1 }

variabledecl:
| IDENT deftype { $1, $2 }

%inline telement:
| TYPE IDENT { $1, $2 }

template:
| TEMPLATE LANGLE separated_nonempty_list(COMMA, telement) RANGLE
  { { tmp_template = $1;
      tmp_langle = $2;
      tmp_args = $3;
      tmp_rangle = $4
    }
  }

fcndef:
| EXPORT? DEF template? IDENT fcntype { $1, $2, $3, $4, $5 }

exprlist:
| separated_nonempty_list(COMMA, expr) { $1 }

field_init:
| IDENT COLON expr
  {  { ptfi_fname = $1;
       ptfi_array = None;
       ptfi_colon = $2;
       ptfi_expr  = $3
     }
  }
| IDENT LSQUARE expr RSQUARE COLON expr
  {  { ptfi_fname = $1;
       ptfi_array = Some ($2, $3, $4);
       ptfi_colon = $5;
       ptfi_expr  = $6
     }
  }

struct_init:
| LCURLY separated_nonempty_list(COMMA, field_init) RCURLY
    { $1, $2, $3 }

expr:
| NEW deftype struct_init? { PTE_New ($1, $2, $3) }
| NIL { PTE_Nil $1 }
| CAST expr AS deftype { PTE_Cast ($1, $2, $3, $4) }
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
| SPAWN IDENT LPAREN separated_list(COMMA, expr) RPAREN
  { PTE_FcnCall (Some $1, $2, $3, $4, $5) }
| IDENT LPAREN separated_list(COMMA, expr) RPAREN
  { PTE_FcnCall (None, $1, $2, $3, $4) }
| IDENT { PTE_Var $1 }
| LPAREN expr RPAREN { $2 }
| LCURLY exprlist RCURLY { PTE_StaticStruct ($1, $2, $3) }
| expr LSQUARE expr RSQUARE { PTE_Index ($1, $2, $3, $4) }
| expr DOT IDENT { PTE_SelectField ($1, $2, $3) }
| expr INCREMENT { PTE_PostUni ($1, $2) }
| expr DECREMENT { PTE_PostUni ($1, $2) }
| INCREMENT expr %prec PREINCR { PTE_PreUni ($1, $2) }
| DECREMENT expr %prec PREDECR { PTE_PreUni ($1, $2) }
| MINUS expr %prec NEGATIVE { PTE_PreUni ($1, $2) }
| PLUS expr %prec POSITIVE { PTE_PreUni ($1, $2) }
| LNOT expr { PTE_PreUni ($1, $2) }
| BNOT expr { PTE_PreUni ($1, $2) }
| AMPERSAND expr %prec ADDR_OF { PTE_PreUni ($1, $2) }
| expr STAR expr { PTE_Bin ($1, $2, $3) }
| expr SLASH expr { PTE_Bin ($1, $2, $3) }
| expr PERCENT expr { PTE_Bin ($1, $2, $3) }
| expr PLUS expr { PTE_Bin ($1, $2, $3) }
| expr MINUS expr { PTE_Bin ($1, $2, $3) }
| expr DBLLANGLE expr { PTE_Bin ($1, $2, $3) }
| expr DBLRANGLE expr { PTE_Bin ($1, $2, $3) }
| expr LANGLE expr { PTE_Bin ($1, $2, $3) }
| expr RANGLE expr { PTE_Bin ($1, $2, $3) }
| expr LEQ expr { PTE_Bin ($1, $2, $3) }
| expr GEQ expr { PTE_Bin ($1, $2, $3) }
| expr EQUALSEQUALS expr { PTE_Bin ($1, $2, $3) }
| expr BANGEQUALS expr { PTE_Bin ($1, $2, $3) }
| expr AMPERSAND expr { PTE_Bin ($1, $2, $3) }
| expr CARAT expr { PTE_Bin ($1, $2, $3) }
| expr VBAR expr { PTE_Bin ($1, $2, $3) }
| expr DBLAMPERSAND expr { PTE_Bin ($1, $2, $3) }
| expr DBLVBAR expr { PTE_Bin ($1, $2, $3) }
| expr ELLIPSIS expr { PTE_Bin ($1, $2, $3) }
| expr EQUALS expr { PTE_Bin ($1, $2, $3) }
| expr PLUSEQUALS expr { PTE_Bin ($1, $2, $3) }
| expr MINUSEQUALS expr { PTE_Bin ($1, $2, $3) }
| expr STAREQUALS expr { PTE_Bin ($1, $2, $3) }
| expr SLASHEQUALS expr { PTE_Bin ($1, $2, $3) }
| expr PERCENTEQUALS expr { PTE_Bin ($1, $2, $3) }
| expr DBLLANGLEEQUALS expr { PTE_Bin ($1, $2, $3) }
| expr DBLRANGLEEQUALS expr { PTE_Bin ($1, $2, $3) }
| expr AMPERSANDEQUALS expr { PTE_Bin ($1, $2, $3) }
| expr CARATEQUALS expr { PTE_Bin ($1, $2, $3) }
| expr VBAREQUALS expr { PTE_Bin ($1, $2, $3) }
