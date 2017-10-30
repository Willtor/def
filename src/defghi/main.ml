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

open Cmdliner

type output = OUT_DEFI | OUT_C_HEADER

let defghi output_kind files =
  let dot_def = Str.regexp "\\.def$" in
  let convert suffix file =
    file, Str.global_replace dot_def suffix file
  in
  let file_pairs = match output_kind with
    | OUT_DEFI -> List.map (convert ".defi") files
    | OUT_C_HEADER -> List.map (convert ".h") files
  in
  List.iter (fun (f, o) -> prerr_endline (f ^ " -> " ^ o)) file_pairs

(* Input FILE name(s) *)
let files = Arg.(non_empty & pos_all file [] & info [] ~docv:"FILE")

(* -i and -h *)
let output_kind =
  let doc = "Generate DEF interface files (.defi).  This is the default." in
  let defi = OUT_DEFI, Arg.info ["i"] ~doc in

  let doc = "Generate C header files (.h)." in
  let c_header = OUT_C_HEADER, Arg.info ["h"] ~doc in

  Arg.(value & vflag OUT_DEFI [defi; c_header])

let cmd =
  let doc = "generate header or interface files from DEF source files." in
  let man = [
      `S Manpage.s_description;
      `P "DEF Generate Header or Interface takes one or more DEF source files
          (.def) and outputs C-compatible header (.h) or DEF interface (.defi)
          files.  The .defi file is default.";
      `S Manpage.s_bugs;
      `P "Bound to be some.  Report bugs to <willtor@mit.edu>."
    ]
  in
  Term.(const defghi $ output_kind $ files),
  Term.info "defghi" ~version:"v-tbd" ~doc ~exits:Term.default_exits ~man

let () = Term.(exit @@ eval cmd)
