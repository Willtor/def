
type tokendata =
  { td_pos : Lexing.position;  (* Where the token was read. *)
    td_text : string;          (* Plain text of the token. *)
    td_noncode : string list   (* Comments, whitespace, etc. between the
                                  previous token and this one.  This list
                                  is reversed from the order in which it
                                  was read. *)
  }

type pt_field =
  | PT_FieldString of tokendata
  | PT_FieldInt of tokendata * tokendata * int32 * tokendata

(** ISM meta program. *)
type ism =
    (* IsmNode (object, next) *)
  | IsmNode of ism * ism
  | IsmTerm of Lexing.position
  | IsmString of Lexing.position * string
  | IsmBool of Lexing.position * bool
  | IsmChar of Lexing.position * char
  | IsmUChar of Lexing.position * char
  | IsmInt16 of Lexing.position * int32
  | IsmUInt16 of Lexing.position * int32
  | IsmInt32 of Lexing.position * int32
  | IsmUInt32 of Lexing.position * int32
  | IsmInt64 of Lexing.position * int64
  | IsmUInt64 of Lexing.position * int64
  | IsmFloat32 of Lexing.position * float
  | IsmFloat64 of Lexing.position * float
  | IsmIdent of tokendata
  | IsmDefStmts of pt_stmt list
  | IsmDefExpr of tokendata * pt_expr
  | IsmDefIdent of Lexing.position * string
  | IsmBinding of binding

and binding =
  (* BBIsm of value *)
  | BBIsm of ism
  (* BBNative of lambda (evaluator -> bindings -> pos -> args -> ret *)
  | BBNative of
      ((binding Util.symtab -> ism -> ism)
       -> binding Util.symtab
       -> Lexing.position -> ism -> ism)
  (* BBLambda of variables * environment * body *)
  | BBLambda of tokendata list * binding Util.symtab * ism

and ident =
  | IdentTok of tokendata
  | IdentIsm of Lexing.position * ism

and pt_stmt =
  | PTS_ISM_Stmts of pt_stmt list
  | PTS_ISM_DelayedStmts of tokendata * ism

  (* PTS_ISM_Expr: Not really a statement, but objects returned from the
     parser need to be of the "pt_stmt list" type.  This should never
     appear in an actual parsetree. *)
  | PTS_ISM_Expr of pt_expr

  | PTS_Import of tokendata * (tokendata * string) * tokendata
  | PTS_Begin of tokendata * pt_stmt list * tokendata
  | PTS_FcnDefExpr of
      (tokendata option * tokendata * ident * pt_type)
      * tokendata * pt_expr * tokendata
  | PTS_FcnDefBlock of
      (tokendata option * tokendata * ident * pt_type)
      * pt_stmt
  | PTS_FcnDecl of tokendata * tokendata * pt_type * tokendata
  | PTS_Expr of pt_expr * tokendata
  | PTS_Var of tokendata * tokendata list * pt_type * tokendata
  | PTS_VarInit of
      tokendata * tokendata list * pt_type option * tokendata * pt_expr list
      * tokendata
  | PTS_VarInlineStruct of
      tokendata * tokendata
      * (tokendata * pt_type) list
      * tokendata * tokendata * pt_expr * tokendata
  | PTS_VarInlineStructInferred of
      tokendata * tokendata
      * tokendata list
      * tokendata * tokendata * pt_expr * tokendata
  | PTS_DeleteExpr of tokendata * pt_expr * tokendata
  | PTS_RetireExpr of tokendata * pt_expr * tokendata
  | PTS_Transaction of tokendata * tokendata * pt_stmt list
                       * (tokendata * pt_expr * tokendata * pt_stmt list) list
                       * tokendata
  | PTS_IfStmt of
      tokendata * pt_expr * tokendata * pt_stmt list
      * (tokendata * pt_expr * tokendata * pt_stmt list) list
      * (tokendata * pt_stmt list) option
      * tokendata
  | PTS_ForLoop of
      tokendata * pt_forinit option * tokendata * pt_expr * tokendata
      * pt_expr option * tokendata * pt_stmt list * tokendata
  | PTS_ParforLoop of
      tokendata * pt_forinit option * tokendata * pt_expr * tokendata
      * pt_expr option * tokendata * pt_stmt list * tokendata
  | PTS_WhileLoop of
      tokendata * pt_expr * tokendata * pt_stmt list * tokendata
  | PTS_DoWhileLoop of
      tokendata * pt_stmt list * tokendata * tokendata * pt_expr * tokendata
  | PTS_SwitchStmt of
      tokendata * pt_expr * tokendata * pt_case list * tokendata
  | PTS_ReturnExpr of tokendata * pt_expr * tokendata
  | PTS_Return of tokendata * tokendata
  | PTS_Type of
      (tokendata * tokendata option) option * tokendata * ident
      * (tokendata * pt_type) option * tokendata
  | PTS_Goto of tokendata * tokendata * tokendata
  | PTS_Break of tokendata * tokendata
  | PTS_Label of tokendata * tokendata
  | PTS_Continue of tokendata * tokendata
  | PTS_Sync of tokendata * tokendata

and pt_case =
  (* No fall-through: OCaml match-like behavior. *)
  | PTMatchCase of tokendata * pt_expr * tokendata * pt_stmt list
  (* Fall-through: C switch-like behavior. *)
  | PTFallCase of tokendata * pt_expr * tokendata * pt_stmt list

and pt_forinit =
  | PTForInit_Var of tokendata * tokendata * pt_type option
                     * tokendata * pt_expr
  | PTForInit_Expr of pt_expr

and pt_type =
  | PTT_Fcn of tokendata * pt_param list * tokendata * tokendata * pt_type
  | PTT_Volatile of tokendata * pt_type
  | PTT_Name of ident
  | PTT_Ptr of tokendata * pt_type
  | PTT_Array of tokendata * pt_expr option * tokendata * pt_type
  | PTT_Struct of tokendata option * tokendata * (tokendata * pt_type) list
                  * tokendata
  | PTT_StructUnnamed of tokendata option * tokendata * pt_type list
                         * tokendata
  | PTT_Enum of tokendata * tokendata list

and pt_param =
  | PTP_Var of tokendata * pt_type
  | PTP_Type of pt_type
  | PTP_Ellipsis of tokendata

and pt_fcn_call =
  { ptfc_spawn    : tokendata option;
    ptfc_name     : ident;
    ptfc_lparen   : tokendata;
    ptfc_args     : pt_expr list;
    ptfc_rparen   : tokendata
  }

and pt_field_init =
  { ptfi_fname : tokendata;
    ptfi_colon : tokendata;
    ptfi_expr  : pt_expr;
  }

and pt_expr =
  | PTE_IsmExpr of tokendata * ism
  | PTE_New of tokendata * pt_type
               * (tokendata * pt_field_init list * tokendata) option
  | PTE_Nil of tokendata
  | PTE_Type of tokendata * pt_type
  | PTE_I64 of (tokendata * int64)
  | PTE_U64 of (tokendata * int64)
  | PTE_I32 of (tokendata * int32)
  | PTE_U32 of (tokendata * int32)
  | PTE_I16 of (tokendata * int32)
  | PTE_U16 of (tokendata * int32)
  | PTE_I8 of (tokendata * char)
  | PTE_U8 of (tokendata * char)
  | PTE_Bool of (tokendata * bool)
  | PTE_F64 of (tokendata * float)
  | PTE_F32 of (tokendata * float)
  | PTE_String of (tokendata * string)
  | PTE_Wildcard of tokendata
  | PTE_FcnCall of pt_fcn_call
  | PTE_Cast of tokendata * pt_type * tokendata * pt_expr * tokendata
  | PTE_Var of ident
  | PTE_StaticStruct of tokendata option * tokendata * pt_expr list * tokendata
  | PTE_StaticArray of tokendata * pt_expr list * tokendata
  | PTE_Index of pt_expr * tokendata * pt_expr * tokendata
  | PTE_SelectField of pt_expr * tokendata * pt_field
  | PTE_PostUni of pt_expr * (Operator.t * tokendata)
  | PTE_PreUni of (Operator.t * tokendata) * pt_expr
  | PTE_Bin of pt_expr * tokendata option * (Operator.t * tokendata) * pt_expr
  | PTE_TernaryCond of pt_expr * tokendata * pt_expr * tokendata * pt_expr

(** Return a string representation of the ISM. *)
val string_of_ism : ism -> string

(** Return the position of an ISM. *)
val pos_of_ism : ism -> Lexing.position

val pt_type_pos : pt_type -> Lexing.position
val pt_expr_pos : pt_expr -> Lexing.position
