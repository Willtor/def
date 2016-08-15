exception ProcessingError of string

val process_ast : string -> string -> Ast.stmt list -> unit
