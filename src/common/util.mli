
type 'a symtab

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

(** Iterate through all symbols in a symtab.  This starts with the inner-most
    scope and proceeds outwards towards global. *)
val symtab_iter : (string -> 'b -> unit) -> 'b symtab -> unit

(** Iterate through all symbols in a symtab and perform a filter (like the
    Hashtbl.filter_map_inplace function, except it returns a new symtab). *)
val symtab_filter : (string -> 'b -> 'b option) -> 'b symtab -> 'b symtab

(****************************************************************************)
(*                            General Utilities                             *)
(****************************************************************************)

(** Generate a random hex string. *)
val random_hex : unit -> string

(** For when an option isn't really an option. *)
val the : 'a option -> 'a

(** Set the reference to the given value, but only if it has never been set
    before.  Return success or non-success. *)
val ref_set : 'a ref -> 'a -> bool ref -> bool

(** Return a unique id string. *)
val unique_id : unit -> string

(** Non-existant position, for when a position is required but not used. *)
val faux_pos : Lexing.position

(** find_path_to ~follow_symlinks file dirlist: finds the first path to the
    specified file given the list of directories.  Optionally follow symbolic
    links. *)
val find_path_to : ?follow_symlinks:bool -> string -> string list -> string

(** Convert a string to a list of chars. *)
val explode_string : string -> char list

(** Combine two lists, but use the provided err function in case of failure *)
val err_combine : (unit -> ('a * 'a) list) -> 'a list -> 'a list
                  -> ('a * 'a) list

(** Apply a function to the object contained in an option variable. *)
val option_map : ('a -> 'b) -> 'a option -> 'b option

(** Combine 3 lists. *)
val combine3 : 'a list -> 'b list -> 'c list -> ('a * 'b * 'c) list

(** Find the position of an element in a list. *)
val whereis : 'a list -> 'a -> int

(** Generate a label string from a position. *)
val label_of_pos : Lexing.position -> string
