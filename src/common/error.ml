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

open Lexing

let color_red_bold = "\x1b[31;1m"
let color_cyan_bold = "\x1b[36;1m"
let color_yellow = "\x1b[33m"
let color_reset = "\x1b[0m"

(** Take a Lexing.position and generate a string of the form:
    "path/to/file.def (line n column n)" *)
let format_position pos =
  pos.pos_fname
  ^ " (line " ^ (string_of_int pos.pos_lnum)
  ^ " column " ^ (string_of_int (pos.pos_cnum - pos.pos_bol))
  ^ ")"

(** Take a Lexing.position and return a 2-line string: the source line, and
    a line with a carat underneath the offending column. *)
let show_source pos =
  try
    let infile = open_in pos.pos_fname in
    let rec find_line n =
      if n > 0 then
        let _ = input_line infile
        in find_line (n - 1)
      else input_line infile
    in
    let str =
      color_yellow ^ (find_line (pos.pos_lnum - 1)) ^ "\n"
      ^ (String.make (pos.pos_cnum - pos.pos_bol) ' ') ^ "^" ^ color_reset
    in
    close_in infile;
    str
  with _ -> "(Err: Unable to read from " ^ pos.pos_fname ^ ")"

(** Report a fatal error with the input string and exit with an error
    condition. *)
let fatal_error err =
  prerr_endline (color_red_bold ^ "Error:\n" ^ color_reset ^ err);
  exit 1

(** Report a non-fatal warning. *)
let warning warn =
  prerr_endline (color_cyan_bold ^ "Warning:\n" ^ color_reset ^ warn)

(** Common pattern for error messages.  Make them uniform. *)
let err_pos msg pos =
  (* FIXME: Format msg to line break at 80 characters. *)
  let err = "At " ^ (format_position pos) ^ ":\n"
            ^ "  " ^ msg ^ "\n"
            ^ (show_source pos)
  in fatal_error err

(** Common pattern for warnings.  Make them uniform. *)
let warn_pos msg pos =
  (* FIXME: Format msg to line break at 80 characters. *)
  let warn = "At " ^ (format_position pos) ^ ":\n"
             ^ "  " ^ msg ^ "\n"
             ^ (show_source pos)
  in warning warn
