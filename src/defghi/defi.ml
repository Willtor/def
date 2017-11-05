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

let the = function
  | Some a -> a
  | None -> Error.fatal_error "the"

let autogen =
  "THIS FILE WAS AUTOMATICALLY GENERATED.  DO NOT MODIFY IT OR YOUR\n"
  ^ " * CHANGES MAY GET CLOBBERED."

let dump_doc oc tok =
  let re = Str.regexp "^/\\*\\*" in
  try
    output_string
      oc
     (List.find (fun s -> Str.string_match re s 0) tok.td_noncode);
    output_string oc "\n"
  with _ -> ()

let rec output_deftype oc =
  let rec print_type width = function
    | PTT_Fcn (_, params, _, _, ret) ->
       let print_params init = function
         | PTP_Var (name, tp) ->
            (output_string oc (init ^ name.td_text ^ " ");
             print_type width tp;
             ", ")
         | PTP_Type tp ->
            (output_string oc init;
             print_type width tp;
             ", ")
         | PTP_Ellipsis _ ->
            (output_string oc (init ^ "...");
             "ERROR - SHOULD NEVER SEE THIS IN OUTPUT")
       in
       begin
         output_string oc "(";
         ignore(List.fold_left print_params "" params);
         output_string oc ") -> ";
         print_type width ret;
       end
    | PTT_Volatile (_, tp) ->
       begin
         output_string oc "volatile ";
         print_type width tp
       end
    | PTT_Name name ->
       output_string oc name.td_text
    | PTT_Ptr (_, tp) ->
       begin
         output_string oc "*";
         print_type width tp
       end
    | PTT_Array (_, expr_maybe, _, tp) ->
       begin
         output_string oc "[";
         if expr_maybe <> None then
           output_expr oc width (the expr_maybe);
         output_string oc "]";
         print_type width tp
       end
    | PTT_Struct (_, members, _) ->
       begin
         output_string oc "{";
         ignore(List.fold_left
                  (fun init (nm, tp) ->
                    output_string oc (init ^ nm.td_text ^ " ");
                    print_type width tp;
                    ", ")
                  ""
                  members);
         output_string oc "}"
       end
    | PTT_StructUnnamed (_, members, _) ->
       begin
         output_string oc "{";
         ignore(List.fold_left
                  (fun init tp ->
                    output_string oc init;
                    print_type width tp;
                    ", ")
                  ""
                  members);
         output_string oc "}"
       end
  in
  print_type

and output_expr oc width = function
  | _ -> Error.fatal_error "don't support expressions, yet."

let output_exported_type oc = function
  | PTS_Type (Some (export, opacity),
              _,
              typename,
              _,
              deftype,
              _) ->
     begin
       dump_doc oc export;
       output_string oc ("typedef " ^ typename.td_text);
       if opacity = None then
         begin
           output_string oc " = ";
           output_deftype oc "  " deftype;
         end;
       output_string oc ";\n\n"
     end
  | _ -> ()

let output_exported_func oc = function
  | PTS_FcnDefExpr ((Some export, _, name, _, tp), _, _, _)
  | PTS_FcnDefBlock ((Some export, _, name, _, tp), _) ->
     begin
       dump_doc oc export;
       output_string oc ("decl " ^ name.td_text ^ " ");
       output_deftype oc "  " tp;
       output_string oc ";\n\n"
     end
  | _ -> ()

let make_defi stmts outfile =
  let oc = open_out outfile in
  output_string oc ("/* " ^ outfile ^ ": " ^ autogen ^ "\n */\n\n");
  List.iter (output_exported_type oc) stmts;
  List.iter (output_exported_func oc) stmts;
  close_out oc
