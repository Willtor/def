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

(** Output an autogen comment alerting programmers that the file was generated
    automatically. *)
let output_autogen oc filename =
  output_string
    oc
    ("/* " ^ filename ^ ":\n"
     ^ " * THIS FILE WAS AUTOMATICALLY GENERATED.  DO NOT MODIFY IT OR YOUR\n"
     ^ " * CHANGES MAY GET CLOBBERED.\n"
     ^ " */\n\n")

let the = function
  | Some a -> a
  | None -> Error.fatal_error "the"

(** Dump doxygen-style documentation from the given token, if any exists. *)
let dump_doc oc tok =
  let re = Str.regexp "^/\\*\\*" in
  try
    output_string
      oc
      (List.find (fun s -> Str.string_match re s 0) tok.td_noncode);
    output_string oc "\n"
  with _ -> ()
