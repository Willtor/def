(** Generate a parse tree from the given input channel/filename. *)
val from_in_channel : string -> in_channel -> Parsetree.pt_stmt list
