(* Copyright (C) 2018  DEFC Authors

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

type record_kind =
  | CR_Struct
  | CR_Union

type ctype =
  | CT_TypeName of Lexing.position * string
  | CT_Pointer of Lexing.position * ctype
  | CT_Record of record_kind * (Lexing.position * string * ctype) list
  | CT_Function of Lexing.position * ctype list * bool * ctype
  | CT_Array of Lexing.position * ctype * int

type cvalue =
  | CV_Function of Lexing.position * string * ctype list * bool * ctype
  | CV_Typedecl of Lexing.position * string * ctype option
  | CV_Variable of Lexing.position * string * ctype * (*extern=*)bool

(** import_c_file f p: Import the C header file, f, using the include
    paths, p, and return a set of type and function declarations. *)
external import_c_file :
  string -> string list -> cvalue list
  = "cimport_import_c_file"
