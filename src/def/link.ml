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

open Config

(* Copied from "clang -v".  Need to figure out how it generates all these
   paths. *)
let ld_cmd = "/usr/bin/ld -z relro --hash-style=gnu --build-id --eh-frame-hdr -m elf_x86_64 -dynamic-linker /lib64/ld-linux-x86-64.so.2"

let split_on_char c str =
  let rec break accum start =
    try let n = String.index_from str start c in
        break ((n + 1) :: accum) (n + 1)
    with _ -> List.rev (((String.length str) + 1) :: accum)
  in
  let breaks = break [0] 0 in
  let rec substrings accum = function
    | start :: stop :: rest ->
       let len = stop - start - 1 in
       let substr = String.sub str start len in
       substrings (substr :: accum) (stop :: rest)
    | _ -> List.rev accum
  in
  substrings [] breaks

let env_paths = try split_on_char ':' (Unix.getenv "LIBRARY_PATH")
                with _ -> []

(** Get the default version of GCC on the system -- we use its libraries and
    runtime for DEF. *)
let gcc_path_and_version () =
  let paths = split_on_char ':' (Unix.getenv "PATH") in
  let gcc_path =
    List.find
      (fun path ->
        try (ignore(Unix.stat (path ^ "/gcc")); true)
        with _ -> false)
      paths
  in
  let gcc = gcc_path ^ "/gcc" in
  let g_stdout, g_stdin, g_stderr = Unix.open_process_full (gcc ^ " -v") [| |] in
  let rec proc_lines () =
    let line = try input_line g_stderr
               with _ -> Report.err_internal __FILE__ __LINE__
                                             "unable to find gcc version."
    in
    try Scanf.sscanf line "gcc version %d.%d.%d"
                     (fun maj min build -> maj, min, build)
    with _ -> proc_lines ()
  in
  let maj, min, build = proc_lines () in
  close_out g_stdin;
  close_in g_stdout;
  close_in g_stderr;
  gcc_path, ((string_of_int maj) ^ "."
             ^ (string_of_int min) ^ "."
             ^ (string_of_int build))

(** Get the basic paths, libraries, and ofiles used by GCC.
    TBD: I don't know how these are actually generated.  Look at clang. *)
let build_paths base_path gcc_ver =
  let def_path = Filename.dirname Sys.executable_name in
  let gcc_libpath = base_path ^ "/../lib/gcc/x86_64-linux-gnu/" ^ gcc_ver in
  let opath = base_path ^ "/../lib/x86_64-linux-gnu" in
  let pathset = [ opath ^ "/crt1.o";
                  opath ^ "/crti.o";
                  gcc_libpath ^ "/crtbegin.o" ]
  and libpaths = env_paths @
                   [ def_path ^ "/../lib";
                     gcc_libpath;
                     opath;
                     "/lib/x86_64-linux-gnu";
                     "/lib64";
                     "/usr/lib/x86_64-linux-gnu";
                     base_path ^ "../lib";
                     "/usr/lib/llvm-3.9/lib"; (* FIXME: too specific. *)
                     "/lib";
                     "/usr/lib" ]
  and libs = [ "-lforkscan";
               "-lhytm";
               "-lcilkrts";
               "-lgcc";
               "--as-needed";
               "-lgcc_s";
               "--no-as-needed";
               "-lc";
               "-lgcc";
               "--as-needed";
               "-lgcc_s";
               "--no-as-needed";
               gcc_libpath ^ "/crtend.o";
               opath ^ "/crtn.o" ]
             @ (List.map (fun l -> "-l" ^ l) !linked_libs)
  in
  String.concat " " (pathset @ (List.map (fun s -> "-L" ^ s) libpaths)),
  String.concat " " libs

let do_linking outfile objects =
  let base_path, gcc_ver = gcc_path_and_version () in
  let ld_paths, ld_libs = build_paths base_path gcc_ver in
  let object_str = String.concat " " objects in
  let ld_in, ld_out =
    Unix.open_process
      (ld_cmd ^ " -o " ^ outfile ^ " " ^ ld_paths ^ " " ^ object_str
       ^ " " ^ ld_libs)
  in
  close_out ld_out;
  close_in ld_in
