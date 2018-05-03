type cfg_expr =
  | Expr_New of Types.deftype * cfg_expr * (int * cfg_expr) list
  | Expr_FcnCall of string * cfg_expr list
  | Expr_FcnCall_Refs of string * (*args=*)string list
  | Expr_String of string * string (* label, contents *)
  | Expr_Binary of Lexing.position * Operator.t * bool * Types.deftype
                   * cfg_expr * cfg_expr
  | Expr_Unary of Operator.t * Types.deftype * cfg_expr * (*pre_p*)bool
  | Expr_Literal of Ast.literal
  | Expr_Variable of string
  | Expr_Cast of Types.deftype * Types.deftype * cfg_expr
  | Expr_Index of cfg_expr * cfg_expr * Types.deftype
                  * (*deref_base=*)bool * (*array=*)bool
                  * (*is_volatile=*)bool
  | Expr_SelectField of cfg_expr * int * (*is_volatile=*)bool
  | Expr_StaticStruct of string option * (Types.deftype * cfg_expr) list
  | Expr_StaticArray of cfg_expr list
  | Expr_Nil
  | Expr_Wildcard
  | Expr_Atomic of atomic_op * (Types.deftype * cfg_expr) list
  | Expr_Val_Ref of string

and atomic_op =
  | AtomicCAS
  | AtomicSwap

type cfg_basic_block =
  | BB_Seq of string * sequential_block
  | BB_Cond of string * conditional_block
  | BB_Term of string * terminal_block
  | BB_Detach of string * string * detach_block
  | BB_Reattach of string * string * sequential_block
  | BB_Sync of string * string * sequential_block
  | BB_Goto of string * sequential_block
  | BB_Error

and sequential_block =
  { mutable seq_prev  : cfg_basic_block list;
    mutable seq_next  : cfg_basic_block;
    mutable seq_expr  : (Lexing.position * cfg_expr) list;
    mutable seq_mark_bit : bool
  }

and conditional_block =
  { mutable cond_prev : cfg_basic_block list;
    mutable cond_next : cfg_basic_block;
    mutable cond_else : cfg_basic_block;
    cond_branch       : (Lexing.position * cfg_expr);
    mutable cond_mark_bit : bool;
    mutable cond_parallel : bool
  }

and terminal_block =
  { mutable term_prev : cfg_basic_block list;
    term_expr         : (Lexing.position * cfg_expr) option;
    mutable term_mark_bit : bool;
    term_xend         : bool
  }

and detach_block =
  { mutable detach_prev : cfg_basic_block list;
    detach_args : (string * cfg_expr) list;
    detach_ret  : (string * cfg_expr) option;
    mutable detach_next : cfg_basic_block;
    mutable detach_continuation : cfg_basic_block;
    mutable detach_mark_bit : bool
  }

and decl =
  { decl_pos   : Lexing.position;
    mappedname : string;
    vis        : Types.visibility;
    is_tls     : bool;
    tp         : Types.deftype;
    params     : (Lexing.position * string) list; (* Zero-len for non-fcns *)
    mutable decl_ref : bool (* Whether this symbol is referenced. *)
  }

and function_defn =
  { defn_begin : Lexing.position;
    defn_end   : Lexing.position;
    name       : string;
    local_vars : (string * decl) list;
    mutable entry_bb : cfg_basic_block;
    fcn_cilk_init : string list
  }

type cfg_scope =
  | ScopeGlobal of Lexing.position
  | ScopeLexical of Lexing.position
  | ScopeLeaf of Lexing.position

type program =
  { global_decls : decl Util.symtab;
    initializers : (string, cfg_expr) Hashtbl.t;
    fcnlist : function_defn list;
    deftypemap : Types.deftype Util.symtab;
    scope_table : (cfg_scope, cfg_scope) Hashtbl.t
  }

(* Visit a graph, depth-first. *)
val visit_df :
  ('a -> cfg_basic_block -> 'a) -> bool -> 'a -> cfg_basic_block -> 'a

(* Reset the marked bit throughout a CFG. *)
val reset_bbs : cfg_basic_block -> unit

val of_ast : Ast.stmt list -> program
