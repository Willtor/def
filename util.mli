
type 'a symtab

(** Take a Lexing.position and generate a string of the form:
    "path/to/file.def (line n column n)" *)
val format_position : Lexing.position -> string

(** Take a Lexing.position and return a 2-line string: the source line, and
    a line with a carat underneath the offending column. *)
val show_source : Lexing.position -> string

(****************************************************************************)
(*                          Symbol Table Functions                          *)
(****************************************************************************)

(** Create a new symbol table with a global scope. *)
val make_symtab : unit -> 'a symtab

(** Push a scope level onto a symbol table. *)
val push_symtab_scope : 'a symtab -> 'a symtab

(** Add a symbol to the symbol table at the deepest scope. *)
val add_symbol : 'a symtab -> string -> 'a -> unit

(** Lookup a symbol at the deepest scope-only.  This is useful for checking
    for redefinitions of a variable when shadowing is okay. *)
val lookup_symbol_local : 'a symtab -> string -> 'a option

(** Lookup a symbol, starting from the deepest scope and working outward. *)
val lookup_symbol : 'a symtab -> string -> 'a option
