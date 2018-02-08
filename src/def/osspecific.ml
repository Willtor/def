(* Copyright (C) 2018  DEFC Authors

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

open Util

let last_nontrivial_entry dirhandle =
  let rec last entry =
    try let next = Unix.readdir dirhandle in
        if next = ".." || next = "." then
          last entry
        else last next
    with _ -> entry
  in
  last ""

let default_import_dirs () =
  let clang_include =
    try
      let path_dirs = String.split_on_char ':' (Sys.getenv "PATH") in
      let clang = find_path_to ~follow_symlinks:true "clang" path_dirs in
      let clang_install_dir = Filename.dirname clang in
      let lib_clang = clang_install_dir ^ "/../lib/clang" in
      let dir = Unix.opendir lib_clang in
      let latest_version = last_nontrivial_entry dir in
      let () = Unix.closedir dir in
      lib_clang ^ "/" ^ latest_version ^ "/include"
    with _ -> Report.err_unable_to_find_clang ()
  in
  [ "/usr/lib/def";
    "/usr/local/lib/def";

    (* C include paths. *)
    "/usr/local/include";
    clang_include;
    (* like: "/usr/lib/llvm-4.0/bin/../lib/clang/4.0.1/include"; *)
    "/usr/include/x86_64-linux-gnu";
    "/usr/include"
  ]
