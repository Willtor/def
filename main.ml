open Ast
open Defparse
open Deflex

let rec depthify depth str =
  if depth <= 0 then str
  else depthify (depth - 1) (" " ^ str)

let rec unparse_stmt depth = function
  | StmtExpr e ->
     begin
       print_string (depthify depth "");
       unparse_expr e;
       print_endline ";"
     end
  | Block slist ->
     begin
       print_endline (depthify depth "{");
       List.iter (fun s -> unparse_stmt (depth + 2) s) slist;
       print_endline (depthify depth "}")
     end
  | DefFcn (f, Block slist) ->
     begin
       unparse_deffcn depth f;
       unparse_stmt depth (Block slist)
     end
  | DefFcn (f, stmt) ->
     begin
       unparse_deffcn depth f;
       print_endline (depthify depth "{");
       unparse_stmt (depth + 2) stmt;
       print_endline (depthify depth "}")
     end
  | Return e ->
     begin
       print_string (depthify depth "return ");
       unparse_expr e;
       print_endline ";"
     end

and unparse_deffcn depth = function
  | NamedFunction (_, nm, params, rettype) ->
     let rec string_of_params str = function
       | [] -> str
       | [(_, var, vartype)] -> str ^ (unparse_type var vartype)
       | (_, var, vartype) :: rest ->
          string_of_params (str ^ (unparse_type var vartype) ^ ", ") rest
     in
     let fcn = nm ^ "(" ^ (string_of_params "" params) ^ ")" in
     let def = unparse_type fcn rettype in
     print_endline (depthify depth def)

and unparse_type var = function
  | VarType (_, str) -> str ^ " " ^ var

and unparse_expr_wrapper e =
  print_string "(";
  unparse_expr e;
  print_string ")"

and unparse_expr = function
  | ExprFcnCall (_, fcn, params) ->
     let rec print_params = function
       | [] -> ()
       | [param] -> unparse_expr param
       | param :: rest ->
          begin
            unparse_expr param;
            print_string ", ";
            print_params rest
          end
     in begin
       print_string (fcn ^ "(");
       print_params params;
       print_string ")"
     end
  | ExprString (_, s) -> print_string s
  | ExprBinary (op, e1, e2) ->
     begin
       unparse_expr_wrapper e1;
       print_string (" " ^ (operator2string op) ^ " ");
       unparse_expr_wrapper e2
     end
  | ExprPreUnary (op, e) ->
     begin
       print_string (operator2string op);
       unparse_expr_wrapper e
     end
  | ExprPostUnary (op, e) ->
     begin
       unparse_expr_wrapper e;
       print_string (operator2string op)
     end
  | ExprAtom (AtomInt (_, i)) -> print_int i
  | ExprAtom (AtomVar (_, s)) -> print_string s

let rec unparse = function
  | s :: rest -> unparse_stmt 0 s; unparse rest
  | [] -> ()

let main () =
  try
    let stmts = ((defparse deflex) (Lexing.from_channel stdin))
    in (*List.iter (fun s -> print_endline (Ast.stmt2string s)) stmts*)
    List.iter (fun s -> unparse_stmt 0 s) stmts
  with LexError err ->
    print_endline err; exit 1

let _ = main ()

