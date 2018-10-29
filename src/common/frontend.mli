(** Generate a parse tree from the given input channel/filename. *)
val from_in_channel : string -> in_channel
                      -> (string, Parsetree.binding) Hashtbl.t
                         * Parsetree.pt_stmt list
