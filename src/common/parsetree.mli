
type tokendata =
  { td_pos : Lexing.position;  (* Where the token was read. *)
    td_text : string;          (* Plain text of the token. *)
    td_noncode : string list   (* Comments, whitespace, etc. between the
                                  previous token and this one.  This list
                                  is reversed from the order in which it
                                  was read. *)
  }

type pt_template =
  { tmp_backtick : tokendata;                     (* ` *)
    tmp_lparen   : tokendata;                     (* ( *)
    tmp_args     : (tokendata * tokendata) list;  (* type T, type U, <etc.> *)
    tmp_rparen   : tokendata                      (* ) *)
  }

type pt_stmt =
  | PTS_Import of tokendata * (tokendata * string) * tokendata
  | PTS_Begin of tokendata * pt_stmt list * tokendata
  | PTS_FcnDefExpr of
      (tokendata option * tokendata * tokendata * pt_template option * pt_type)
      * tokendata * pt_expr * tokendata
  | PTS_FcnDefBlock of
      (tokendata option * tokendata * tokendata * pt_template option * pt_type)
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
  | PTS_Transaction of tokendata * pt_stmt list * tokendata
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
      (tokendata * tokendata option) option * tokendata * tokendata
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
  | PTT_Name of tokendata
  | PTT_Ptr of tokendata * pt_type
  | PTT_Array of tokendata * pt_expr option * tokendata * pt_type
  | PTT_Struct of tokendata * (tokendata * pt_type) list * tokendata
  | PTT_StructUnnamed of tokendata * pt_type list * tokendata
  | PTT_Enum of tokendata * tokendata list

and pt_param =
  | PTP_Var of tokendata * pt_type
  | PTP_Type of pt_type
  | PTP_Ellipsis of tokendata

and pt_fcn_call =
  { ptfc_spawn    : tokendata option;
    ptfc_name     : tokendata;
    ptfc_template : pt_template_inst option;
    ptfc_lparen   : tokendata;
    ptfc_args     : pt_expr list;
    ptfc_rparen   : tokendata
  }

and pt_template_inst =
  { ptti_bt     : tokendata;
    ptti_lparen : tokendata option;
    ptti_args   : pt_type list;
    ptti_rparen : tokendata option
  }

and pt_field_init =
  { ptfi_fname : tokendata;
    ptfi_array : (tokendata * pt_expr * tokendata) option;
    ptfi_colon : tokendata;
    ptfi_expr  : pt_expr;
  }

and pt_expr =
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
  | PTE_Var of tokendata
  | PTE_StaticStruct of tokendata * pt_expr list * tokendata
  | PTE_StaticArray of tokendata * pt_expr list * tokendata
  | PTE_Index of pt_expr * tokendata * pt_expr * tokendata
  | PTE_SelectField of pt_expr * tokendata * tokendata
  | PTE_PostUni of pt_expr * tokendata
  | PTE_PreUni of tokendata * pt_expr
  | PTE_Bin of pt_expr * tokendata option * tokendata * pt_expr

val pt_type_pos : pt_type -> Lexing.position
val pt_expr_pos : pt_expr -> Lexing.position
