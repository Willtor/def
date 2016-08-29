
open Lexing

type 'a symtab = (string, 'a) Hashtbl.t list

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
    (find_line (pos.pos_lnum - 1)) ^ "\n"
    ^ (String.make (pos.pos_cnum - pos.pos_bol) ' ') ^ "^"
  with _ -> "(Err: Unable to read from " ^ pos.pos_fname ^ ")"

(** Report a fatal error with the input string and exit with an error
    condition. *)
let fatal_error err =
  prerr_endline ("Error:\n" ^ err);
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

(****************************************************************************)
(*                            General Utilities                             *)
(****************************************************************************)

(** For when an option isn't really an option. *)
let the = function
  | Some v -> v
  | None -> failwith "Internal error.  Expected a value from Util.the."
