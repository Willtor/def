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
open Unix

type 'a symtab = (string, 'a) Hashtbl.t list

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

(** find_path_to ~follow_symlinks file dirlist: finds the first path to the
    specified file given the list of directories.  Optionally follow symbolic
    links. *)
let find_path_to ?follow_symlinks:(follow_symlinks=false) file dirlist =
  let rec find_in = function
    | [] -> raise Not_found
    | dir :: rest ->
       let candidate = dir ^ "/" ^ file in
       if Sys.file_exists candidate then candidate
       else find_in rest
  in
  let rec read_symlinks f =
    let stat = Unix.lstat f in
    if stat.st_kind = Unix.S_LNK then
      let linked_f = (Filename.dirname f) ^ "/" ^ (Unix.readlink f) in
      read_symlinks linked_f
    else f
  in
  let f =
    if file.[0] = '/' then file
    else
      find_in dirlist
  in
  if follow_symlinks then read_symlinks f
  else f

(** Convert a string to a list of chars. *)
let explode_string s =
  let rec explode n accum =
    if n < 0 then accum
    else explode (n - 1) (s.[n] :: accum)
  in
  explode ((String.length s) - 1) [Char.chr 0]

(** Combine two lists, but use the provided err function in case of failure *)
let err_combine err l1 l2 =
  if (List.length l1) <> (List.length l2) then
    err ()
  else
    List.combine l1 l2

(** Apply a function to the object contained in an option variable. *)
let option_map f = function
  | None -> None
  | Some v -> Some (f v)

(** Combine 3 lists. *)
let combine3 a b c =
  let rec combine accum = function
    | [], [], [] -> List.rev accum
    | a1 :: arest, b1 :: brest, c1 :: crest ->
       combine ((a1, b1, c1) :: accum) (arest, brest, crest)
    | _ -> invalid_arg "combine3: inconsistent list lengths"
  in
  combine [] (a, b, c)
