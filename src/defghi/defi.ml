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

let autogen =
  "THIS FILE WAS AUTOMATICALLY GENERATED.  DO NOT MODIFY IT OR YOUR\n"
  ^ " * CHANGES MAY GET CLOBBERED."

let dump_doc oc tok =
  output_string oc "/** DEFGHI FIXME: Some doc. */\n"

let output_deftype oc width = function
  | _ -> output_string oc "some type"

let output_type oc = function
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
           (* FIXME: WORKING HERE! *)
           output_deftype oc "  " deftype;
         end;
       output_string oc ";\n"
     end
  | _ -> ()

let make_defi stmts outfile =
  let oc = open_out outfile in
  output_string oc ("/* " ^ outfile ^ ": " ^ autogen ^ "\n */\n");
  List.iter (output_type oc) stmts;
  close_out oc
