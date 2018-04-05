(* Copyright (C) 2017  DEF Authors

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

open Parsetree
open Lexing
open Util

(* Tab-width. FIXME: This should be configurable. *)
let tab = "  "

(* Map of corresponding builtin types for DEF and C. *)
let builtin_map =
  let type_pairs =
    [ "bool", "char";
      "uchar", "unsigned char";
      "i8", "char";
      "u8", "unsigned char";
      "i16", "short";
      "u16", "unsigned short";
      "i32", "int";
      "u32", "unsigned int";
      "i64", "long long";
      "u64", "unsigned long long";
      "f32", "float";
      "f64", "double"
    ]
  in
  let map = Hashtbl.create 11 in
  List.iter (fun (a, b) -> Hashtbl.add map a b) type_pairs;
  map

let rec output_deftype oc name depth tp =
  let rec build_string accum depth = function
    | PTT_Fcn (_, params, _, _, ret) ->
       let param2string = function
         | PTP_Var (name, tp) -> build_string name.td_text "" tp
         | PTP_Type tp -> build_string "" "" tp
         | PTP_Ellipsis _ -> "..."
       in
       let pstrings = List.map param2string params in
       let non_ret = accum ^ " " ^ "(" ^ (String.concat ", " pstrings) ^ ")" in
       build_string non_ret depth ret
    | PTT_Volatile (_, tp) ->
       (* FIXME: Pretty sure this is wrong.  Fix it later.  Other stuff to do. *)
       build_string ("volatile " ^ accum) depth tp
    | PTT_Name nm ->
       let converted_name = try Hashtbl.find builtin_map nm.td_text
                            with _ -> nm.td_text
       in
       converted_name ^ " " ^ accum
    | PTT_Ptr (_, tp) ->
       build_string ("*" ^ accum) depth tp
    | PTT_Array (_, expr, _, tp) ->
       Error.fatal_error "No arrays, yet."
    | PTT_Struct (_, members, _) ->
       let proc_member accum (nm, tp) =
         let member = build_string nm.td_text (tab ^ depth) tp in
         accum ^ tab ^ depth ^ member ^ ";\n"
       in
       let body = List.fold_left proc_member "" members in
       "struct " ^ accum ^ "\n" ^ depth ^ "{\n" ^ body ^ depth ^ "};\n"
    | PTT_StructUnnamed (_, members, _) ->
       let proc_member accum tp =
         let member = build_string "" (tab ^ depth) tp in
         accum ^ depth ^ member ^ ";\n"
       in
       let body = List.fold_left proc_member "" members in
       "struct\n" ^ depth ^ "{\n" ^ body ^ depth ^ "};\n"
    | PTT_Enum (_, opts) ->
       let opt_strings = List.map (fun tok -> tok.td_text) opts in
       "enum { " ^ (String.concat ", " opt_strings)
       ^ " } " ^ accum ^ ";\n"
  in
  let str = build_string name depth tp in
  output_string oc str

let output_exported_typedef oc = function
  | PTS_Type (Some (export, opacity),
              _,
              typename,
              _,
              PTT_Struct _,
              _) ->
     begin
       if opacity <> None then
         dump_doc oc export;
       output_string oc ("typedef struct "
                         ^ typename.td_text ^ " "
                         ^ typename.td_text ^ ";\n\n")
     end
  | PTS_Type (Some (export, opacity),
              _,
              typename,
              _,
              deftype,
              _) ->
       if opacity <> None then
         dump_doc oc export;
       begin
         output_string oc "typedef ";
         output_deftype oc typename.td_text "" deftype;
         output_string oc ";\n\n"
       end
  | _ -> ()

let output_exported_type oc = function
  | PTS_Type (Some (export, None),
              _,
              typename,
              _,
              (PTT_Struct (_, members, _) as tp),
              _) ->
     begin
       dump_doc oc export;
       output_deftype oc typename.td_text "" tp;
       output_string oc "\n"
     end
  | _ -> ()

let output_exported_function oc = function
  | PTS_FcnDefExpr ((Some export, _, name, _, deftype), _, _, _)
  | PTS_FcnDefBlock ((Some export, _, name, _ , deftype), _) ->
     begin
       dump_doc oc export;
       output_deftype oc name.td_text "" deftype;
       output_string oc ";\n\n"
     end
  | _ -> ()

let make_header stmts outfile =
  let oc = open_out outfile in
  output_autogen oc outfile;
  output_string oc "#pragma once\n";
  output_string oc "#ifdef __cplusplus\n";
  output_string oc "extern \"C\" {\n";
  output_string oc "#endif\n\n";
  List.iter (output_exported_typedef oc) stmts;
  List.iter (output_exported_type oc) stmts;
  List.iter (output_exported_function oc) stmts;
  output_string oc "#ifdef __cplusplus\n";
  output_string oc "} // extern \"C\"\n";
  output_string oc "#endif\n\n";
  close_out oc
