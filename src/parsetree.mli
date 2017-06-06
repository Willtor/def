
type tokendata =
  { td_pos : Lexing.position;  (* Where the token was read. *)
    td_text : string;          (* Plain text of the token. *)
    td_noncode : string list   (* Comments, whitespace, etc. between the
                                  previous token and this one. *)
  }

type pt_stmt =
  | PTS_Begin of tokendata * pt_stmt list * tokendata
  | PTS_FcnDefExpr of
      (tokendata option * tokendata * tokendata * pt_type)
      * tokendata * pt_expr * tokendata
  | PTS_FcnDefBlock of
      (tokendata option * tokendata * tokendata * pt_type) * pt_stmt
  | PTS_FcnDecl of tokendata * tokendata * pt_type * tokendata
  | PTS_Expr of pt_expr * tokendata
  | PTS_Var of tokendata * tokendata list * pt_type * tokendata
  | PTS_VarInit of
      tokendata * tokendata list * pt_type * tokendata * pt_expr list
      * tokendata
  | PTS_VarInlineStruct of
      tokendata * tokendata
      * (tokendata * pt_type) list
      * tokendata * tokendata * pt_expr * tokendata
  | PTS_DeleteExpr of tokendata * pt_expr * tokendata
  | PTS_RetireExpr of tokendata * pt_expr * tokendata
  | PTS_XBegin of tokendata * tokendata
  | PTS_XCommit of tokendata * tokendata
  | PTS_IfStmt of
      tokendata * pt_expr * tokendata * pt_stmt list
      * (tokendata * pt_expr * tokendata * pt_stmt list) list
      * (tokendata * pt_stmt list) option
      * tokendata
  | PTS_ForLoop of
      tokendata * pt_forinit option * tokendata * pt_expr * tokendata
      * pt_expr option * tokendata * pt_stmt list * tokendata
  | PTS_WhileLoop of
      tokendata * pt_expr * tokendata * pt_stmt list * tokendata
  | PTS_DoWhileLoop of
      tokendata * pt_stmt list * tokendata * tokendata * pt_expr * tokendata
  | PTS_ReturnExpr of tokendata * pt_expr * tokendata
  | PTS_Return of tokendata * tokendata
  | PTS_Type of
      (tokendata * tokendata option) option * tokendata * tokendata
      * tokendata * pt_type * tokendata
  | PTS_Goto of tokendata * tokendata * tokendata
  | PTS_Break of tokendata * tokendata
  | PTS_Label of tokendata * tokendata
  | PTS_Continue of tokendata * tokendata

and pt_forinit =
  | PTForInit_Var of tokendata * tokendata * pt_type * tokendata * pt_expr
  | PTForInit_Expr of pt_expr

and pt_type =
  | PTT_Fcn of tokendata * pt_param list * tokendata * tokendata * pt_type
  | PTT_Volatile of tokendata * pt_type
  | PTT_Name of tokendata
  | PTT_Ptr of tokendata * pt_type
  | PTT_Array of tokendata * pt_expr option * tokendata * pt_type
  | PTT_Struct of tokendata * (tokendata * pt_type) list * tokendata
  | PTT_StructUnnamed of tokendata * pt_type list * tokendata

and pt_param =
  | PTP_Var of tokendata * pt_type
  | PTP_Type of pt_type
  | PTP_Ellipsis of tokendata

and pt_expr =
  | PTE_New of
      tokendata * pt_type
      * (tokendata * (tokendata * tokendata * pt_expr) list * tokendata) option
  | PTE_Nil of tokendata
  | PTE_Cast of tokendata * pt_expr * tokendata * pt_type
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
  | PTE_FcnCall of tokendata * tokendata * pt_expr list * tokendata
  | PTE_Var of tokendata
  | PTE_StaticStruct of tokendata * pt_expr list * tokendata
  | PTE_Index of pt_expr * tokendata * pt_expr * tokendata
  | PTE_SelectField of pt_expr * tokendata * tokendata
  | PTE_PostUni of pt_expr * tokendata
  | PTE_PreUni of tokendata * pt_expr
  | PTE_Bin of pt_expr * tokendata * pt_expr

val pt_type_pos : pt_type -> Lexing.position
val pt_expr_pos : pt_expr -> Lexing.position
