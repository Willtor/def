(* Copyright (C) 2018  DEF Authors

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

type t =
  | OperIncr | OperDecr | OperAddrOf
  | OperMinus | OperPlus
  | OperLogicalNot | OperBitwiseNot
  | OperMult | OperDiv | OperRemainder
  | OperLShift | OperRShift
  | OperLT | OperGT | OperLTE
  | OperGTE | OperEquals | OperNEquals
  | OperBitwiseAnd | OperBitwiseXor | OperBitwiseOr
  | OperLogicalAnd | OperLogicalOr
  | OperEllipsis
  | OperAssign | OperPlusAssign
  | OperMinusAssign | OperMultAssign
  | OperDivAssign | OperRemAssign
  | OperLShiftAssign | OperRShiftAssign
  | OperBAndAssign | OperBXorAssign | OperBOrAssign

let string_of_operator = function
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
  | OperEllipsis -> "..."
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
