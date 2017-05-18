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

open Ast
open Lexing
open Types

let autogen =
  "THIS FILE WAS AUTOMATICALLY GENERATED.  DO NOT MODIFY IT OR YOUR\n"
  ^ " * CHANGES MAY GET CLOBBERED."

let begin_cpp_mode =
  "#ifdef __cplusplus\nextern \"C\" {\n#endif\n\n"

let end_cpp_mode =
  "#ifdef __cplusplus\n} // extern \"C\"\n#endif\n"

let ctype_of =
  let map = Hashtbl.create 32 in
  List.iter
    (fun (name, _, _, ctype) -> Hashtbl.add map name ctype)
    Types.map_builtin_types;
  fun name ->
    try Hashtbl.find map name with _ -> name

let repair_str =
  let re = Str.regexp "\\." in
  let repair = Str.global_replace re "_"
  in repair

let name_of_pos pos =
  (repair_str pos.pos_fname) ^ "_"
  ^ (string_of_int pos.pos_lnum) ^ "_"
  ^ (string_of_int (pos.pos_cnum - pos.pos_bol))

let name_of_tuple = function
  | (pos, _, _) :: _ ->
     "__tuple_" ^ (name_of_pos pos)
  | [] -> Report.err_internal __FILE__ __LINE__
                              "tuple had no members."

let rec unparse_expr = function
  | ExprLit (_, LitBool b) -> string_of_bool b
  | ExprLit (_, LitI8 c) | ExprLit (_, LitU8 c) ->
     "'" ^ (Char.escaped c) ^ "'"
  | ExprLit (_, LitI16 n) | ExprLit (_, LitU16 n)
    | ExprLit (_, LitI32 n) | ExprLit (_, LitU32 n) ->
     Int32.to_string n
  | ExprLit (_, LitI64 n) | ExprLit (_, LitU64 n) ->
     Int64.to_string n
  | _ -> Report.err_internal __FILE__ __LINE__
                             "Don't know how to unparse complex exprs."

let rec cstring_of_type accum = function
  | VarType (_, nm, _) -> (ctype_of nm) ^ accum (* FIXME: Report qualifiers *)
  | FcnType _ -> Report.err_internal __FILE__ __LINE__
     "FcnType not implemented."
  | StructType members -> (name_of_tuple members) ^ accum
  | ArrayType (_, e, t) ->
     (cstring_of_type (accum ^ "[" ^ (unparse_expr e) ^ "]") t)
  | PtrType (_, t) -> (cstring_of_type ("*" ^ accum) t)
  | Ellipsis _ -> "..."

let output_typedefs oc = function
  | TypeDecl (_, nm, StructType _, VisExported _, _) ->
     output_string oc ("typedef struct " ^ nm ^ " " ^ nm ^ ";\n\n")
  | TypeDecl (_, nm, _, VisExported _, _) ->
     Report.err_internal __FILE__ __LINE__
                         "Non-struct types not yet supported."
  | DefFcn (_, _, VisExported _, _, FcnType (_, rtype), _) ->
     begin match rtype with
     | StructType members ->
        let nm = name_of_tuple members in
        output_string oc ("typedef struct " ^ nm ^ " " ^ nm ^ ";\n\n")
     | _ -> ()
     end
  | _ -> ()

let output_structs oc = function
  | TypeDecl (_, nm, StructType mlist, VisExported _, false) ->
     begin
       output_string oc ("struct " ^ nm ^ " {\n");
       List.iter
         (fun (_, mname, tp) ->
           output_string oc ("  " ^ (cstring_of_type (" " ^ mname) tp)
                             ^ ";\n"))
         mlist;
       output_string oc "};\n\n"
     end
  | TypeDecl (_, nm, _, VisExported _, false) ->
     Report.err_internal __FILE__ __LINE__
       "Non-struct types not yet supported."
  | DefFcn (_, _, VisExported _, _, FcnType (_, rtype), _) ->
     begin match rtype with
     | StructType members ->
        let nm = name_of_tuple members in
        output_string oc ("struct " ^ nm ^ " {\n");
        List.iter
          (fun (pos, mname, tp) ->
            let mname = if mname = "" then "__member_" ^ (name_of_pos pos)
                        else mname
            in
            output_string oc ("  " ^ (cstring_of_type (" " ^ mname) tp)
                              ^ ";\n"))
          members;
        output_string oc "};\n\n"
     | _ -> ()
     end
  | _ -> ()

let output_functions oc = function
  | DefFcn (_, doc, VisExported _, name, FcnType (args, ret), _) ->
     let () = match doc with
       | None -> ()
       | Some comment ->
          output_string oc (comment ^ "\n")
     in
     begin
       output_string oc (cstring_of_type (" " ^ name) ret);
       output_string oc "(";
       let params =
         List.map (fun (_, name, tp) ->
             cstring_of_type (if name <> "" then " " ^ name else "") tp)
                  args
       in
       output_string oc (String.concat ", " params);
       output_string oc ");\n\n"
     end
  | _ -> ()

(** header_of outfile stmts: Output a C/C++ header file of "outfile" using
    a set of AST stmts. *)
let header_of outfile stmts =
  let oc = open_out outfile in
  output_string oc ("/* " ^ outfile ^ ": " ^ autogen ^ "\n */\n");
  output_string oc "#pragma once\n";
  output_string oc begin_cpp_mode;
  List.iter (output_typedefs oc) stmts;
  List.iter (output_structs oc) stmts;
  List.iter (output_functions oc) stmts;
  output_string oc end_cpp_mode;
  close_out oc
