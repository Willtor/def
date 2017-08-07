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

type 'a symtab = (string, 'a) Hashtbl.t list

let color_red_bold = "\x1b[31;1m"
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
  let infile = open_in pos.pos_fname in
  let rec find_line n =
    if n > 0 then
        let _ = input_line infile
        in find_line (n - 1)
    else input_line infile
  in
  try
    color_yellow ^ (find_line (pos.pos_lnum - 1)) ^ "\n"
    ^ (String.make (pos.pos_cnum - pos.pos_bol) ' ') ^ "^" ^ color_reset
  with _ -> "(Err: Unable to read from " ^ pos.pos_fname ^ ")"

(** Report a fatal error with the input string and exit with an error
    condition. *)
let fatal_error err =
  prerr_endline (color_red_bold ^ "Error:\n" ^ color_reset ^ err);
  exit 1

(** Report a non-fatal warning. *)
let warning warn = prerr_endline ("Warning:\n" ^ warn)

(****************************************************************************)
(*                          Symbol Table Functions                          *)
(****************************************************************************)

let make_symtab_layer () = Hashtbl.create 32

(** Create a new symbol table with a global scope. *)
let make_symtab () =
  [make_symtab_layer ()]

(** Push a scope level onto a symbol table. *)
let push_symtab_scope symtab = (make_symtab_layer ()) :: symtab

(** Add a symbol to the symbol table at the deepest scope. *)
let add_symbol symtab name value =
  match symtab with
  | scope :: _ -> Hashtbl.add scope name value
  | [] -> failwith "Internal error: No symbol table scopes."

(** Lookup a symbol at the deepest scope-only.  This is useful for checking
    for redefinitions of a variable when shadowing is okay. *)
let lookup_symbol_local symtab name =
  match symtab with
  | scope :: _ ->
     begin
       try Some (Hashtbl.find scope name) with Not_found -> None
     end
  | [] -> failwith "Internal error: No symbol table scopes."

(** Lookup a symbol, starting from the deepest scope and working outward. *)
let rec lookup_symbol symtab name =
  match symtab with
  | [] -> failwith "Internal error: No symbol table scopes."
  | [global] -> lookup_symbol_local symtab name
  | scope :: rest ->
     begin match lookup_symbol_local symtab name with
     | Some _ as ret -> ret
     | None -> lookup_symbol rest name
     end

(** Iterate through all symbols in a symtab.  This starts with the inner-most
    scope and proceeds outwards towards global. *)
let symtab_iter f =
  List.iter (fun table -> Hashtbl.iter f table)

(** Iterate through all symbols in a symtab and perform a filter (like the
    Hashtbl.filter_map_inplace function, except it returns a new symtab). *)
let symtab_filter f =
  let process_tbl orig =
    let replacement = Hashtbl.create (Hashtbl.length orig) in
    Hashtbl.iter
      (fun k old_v -> match f k old_v with
      | None -> ()
      | Some new_v -> Hashtbl.add replacement k new_v)
      orig;
    replacement
  in List.map (fun tbl -> process_tbl tbl)

(****************************************************************************)
(*                            General Utilities                             *)
(****************************************************************************)

(** Generate a random hex string. *)
let random_hex =
  Random.self_init ();
  (fun () -> Printf.sprintf "%x" (Random.bits ()))

(** For when an option isn't really an option. *)
let the = function
  | Some v -> v
  | None -> failwith "Internal error.  Expected a value from Util.the."

(** Set the reference to the given value, but only if it has never been set
    before.  Return success or non-success. *)
let ref_set reference v already_set =
  if !already_set then false
  else (already_set := true;
        reference := v;
        true)

(** Return a unique id string. *)
let unique_id =
  let n = ref 0 in
  fun () -> n := !n + 1;
            string_of_int !n

(** Non-existant position, for when a position is required but not used. *)
let faux_pos = { pos_fname = "";
                 pos_lnum = 0;
                 pos_bol = 0;
                 pos_cnum = 0 }
