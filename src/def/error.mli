(** Take a Lexing.position and generate a string of the form:
    "path/to/file.def (line n column n)" *)
val format_position : Lexing.position -> string

(** Take a Lexing.position and return a 2-line string: the source line, and
    a line with a carat underneath the offending column. *)
val show_source : Lexing.position -> string

(** Report a fatal error with the input string and exit with an error
    condition. *)
val fatal_error : string -> 'a

(** Report a non-fatal warning. *)
val warning : string -> unit
