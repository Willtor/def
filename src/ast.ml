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

open Types
open Lexing

type literal =
  | LitBool of bool
  | LitI8  of char
  | LitU8  of char
  | LitI16 of int32
  | LitU16 of int32
  | LitI32 of int32
  | LitU32 of int32
  | LitI64 of int64
  | LitU64 of int64
  | LitF32 of float
  | LitF64 of float

type operator =
  | OperIncr | OperDecr | OperAddrOf
  | OperMinus | OperPlus
  | OperLogicalNot | OperBitwiseNot
  | OperMult | OperDiv | OperRemainder
  | OperLShift | OperRShift
  | OperLT | OperGT | OperLTE
  | OperGTE | OperEquals | OperNEquals
  | OperBitwiseAnd | OperBitwiseXor | OperBitwiseOr
  | OperLogicalAnd | OperLogicalOr
  | OperAssign | OperPlusAssign
  | OperMinusAssign | OperMultAssign
  | OperDivAssign | OperRemAssign
  | OperLShiftAssign | OperRShiftAssign
  | OperBAndAssign | OperBXorAssign | OperBOrAssign

type vartype =
  | VarType of position * string
  | FcnType of (position * string * vartype) list * vartype
  | StructType of (position * string * vartype) list
  | PtrType of position * vartype
  | Ellipsis of position

type fcn_call =
  { fc_pos  : position;
    fc_name : string;
    fc_args : expr list
  }

and operation =
  { op_pos : position;
    op_op : operator;
    op_left : expr;
    op_right : expr option
  }

and expr =
  | ExprFcnCall of fcn_call
  | ExprString of position * string
  | ExprBinary of operation
  | ExprPreUnary of operation
  | ExprPostUnary of operation
  | ExprVar of position * string
  | ExprLit of position * literal
  | ExprCast of position * vartype * expr
  | ExprIndex of position * expr * position * expr
  | ExprSelectField of position * position * expr * string
  | ExprStaticStruct of position * (position * expr) list
  | ExprType of position * vartype
  | ExprNil of position

type stmt =
  | StmtExpr of position * expr
  | Block of position * stmt list
  | DeclFcn of position * Types.visibility * string * vartype
  | DefFcn of position * Types.visibility * string * vartype * stmt list
  | VarDecl of (position * string * (position * expr) option) list * vartype
  | IfStmt of position * expr * stmt list * stmt list option
  (* WhileLoop: start-pos * pre-check * cond * body *)
  | WhileLoop of position * bool * expr * stmt list
  | Return of position * expr
  | ReturnVoid of position
  | TypeDecl of position * string * vartype
  | Label of position * string
  | Goto of position * string
  | Continue of position

let operator2string = function
  | OperIncr -> "++"
  | OperDecr -> "--"
  | OperAddrOf -> "&"
  | OperMinus -> "-"
  | OperPlus -> "+"
  | OperLogicalNot -> "!"
  | OperBitwiseNot -> "~"
  | OperMult -> "*"
  | OperDiv -> "/"
  | OperRemainder -> "%"
  | OperLShift -> "<<"
  | OperRShift -> ">>"
  | OperLT -> "<"
  | OperGT -> ">"
  | OperLTE -> "<="
  | OperGTE -> ">="
  | OperEquals -> "=="
  | OperNEquals -> "!="
  | OperBitwiseAnd -> "&"
  | OperBitwiseXor -> "^"
  | OperBitwiseOr -> "|"
  | OperLogicalAnd -> "&&"
  | OperLogicalOr -> "||"
  | OperAssign -> "="
  | OperPlusAssign -> "+="
  | OperMinusAssign -> "-="
  | OperMultAssign -> "*="
  | OperDivAssign -> "/="
  | OperRemAssign -> "%="
  | OperLShiftAssign -> "<<="
  | OperRShiftAssign -> ">>="
  | OperBAndAssign -> "&="
  | OperBXorAssign -> "^="
  | OperBOrAssign -> "|="

let literal2primitive = function
  | LitBool _ -> PrimBool
  | LitI8  _ -> PrimI8
  | LitU8  _ -> PrimU8
  | LitI16 _ -> PrimI16
  | LitU16 _ -> PrimU16
  | LitI32 _ -> PrimI32
  | LitU32 _ -> PrimU32
  | LitI64 _ -> PrimI64
  | LitU64 _ -> PrimU64
  | LitF32 _ -> PrimF32
  | LitF64 _ -> PrimF64
