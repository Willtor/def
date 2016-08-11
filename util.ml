
open Lexing

(* Take a Lexing.position and generate a string of the form:
   "path/to/file.def (line n column n)" *)
let format_position pos =
  pos.pos_fname
  ^ " (line " ^ (string_of_int pos.pos_lnum)
  ^ " column " ^ (string_of_int (pos.pos_cnum - pos.pos_bol))
  ^ ")"

(* Take a Lexing.position and return a 2-line string: the source line, and a
   line with a carat underneath the offending column. *)
let show_source pos =
  let infile = open_in pos.pos_fname in
  let rec find_line n =
    if n > 0 then
        let _ = input_line infile
        in find_line (n - 1)
    else input_line infile
  in
  try
    (find_line (pos.pos_lnum - 1)) ^ "\n"
    ^ (String.make (pos.pos_cnum - pos.pos_bol) ' ') ^ "^"
  with _ -> "(Err: Unable to read from " ^ pos.pos_fname ^ ")"
