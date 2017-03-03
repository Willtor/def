
(** header_of filename stmts: Output a C/C++ header file of "filename" using
    a set of AST stmts. *)
val header_of : string -> Ast.stmt list -> unit
