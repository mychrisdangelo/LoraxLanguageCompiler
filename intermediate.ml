open Ast
open Check

type inter_var = Ast.var
type inter_var_type = Ast.var_type
type inter_fdecl = Ast.func_decl


type inter_lit = 
  IntLit of int
  | FloatLit of float
  | StringLit of string
  | CharLit of char
  | BoolLit of bool
  | NullLit

type inter_expr =
 Binop of inter_var *  inter_var * Ast.op * inter_var
 | Unop of inter_var * inter_var * Ast.uop
 | Call of inter_var * inter_fdecl * inter_var list
 | Lit of inter_var * inter_lit
 | Tree of inter_var * inter_expr * inter_expr list
 (* We need to talk about the following two cases as a group/I dont really know
  * how this should be handled *)
 | Assign of inter_var * inter_expr * inter_expr (* this is most likely
 incorrect *)

type inter_stmt =
  Decl of inter_var
  | If of inter_var * string (* temporary *)
  | Jmp of string
  | Label of string
  | Ret of inter_var
  | Expr of inter_expr

and inter_func = {
  args: inter_var list;
  header: inter_fdecl;
  code: inter_stmt list;
}

and inter_prototype = {
  name: string;
  ret_type: inter_var_type;
  formals: inter_var list;
}

type inter_pgrm = {
  globals: inter_var list;
  fdecls: inter_fdecl list;
  funcs: inter_func list;
}



and simplify_assign (v:var_type) (e1:c_expr) (e2:c_expr) = function
  (* TODO *)

and simplify_expr (e:c_expr) = function
  (* TODO *)

and simplify_stmt (s:c_stmt) = function
  match s with
  Block(b) -> simplify_block b (* Block type needs to be defined in ast and
  check so that we can separate out local variable declarations *)
  | Expr(e) -> simplify_expr
  | Return(e) -> (* TODO *)
  | If(e, s, s) ->
  | For(e, e, e, s) ->
  | While(e, s) ->
  | Continue ->
  | Break -> 


and simplify_stmtlist (l:c_stmt list) = function 
  [] -> []
  | head::tail -> simple_stmt head:: simplify_stmtlist tail

and simplify_locals (l:var list) = function
  [] -> []
  | _ -> List.map (fun e -> Decl(e)) l 

and simplify_funcs (l:c_func_decl list) = function
  [] -> []
  | head::tail -> simplify_funcs head:: simplify_funcs tail

and simplify_func (f:c_func_decl) = function
  let body = simplify_body f.body in
  let locals = simplify_locals f.locals in
  let head = simplify_prototype f in
  let formals = f.formals in
  {args = formals; header = head; code: locals @ body}


and simplify_prototypes (l:c_func_decl list) = function
  [] -> []
  | head::tail -> simplify_prototype head:: simplify_prototypes tail

and simplify_prototype (f:c_func_decl) = function
  {name = f.fname; ret_type = f.ret_type; formals = f.formals}

let rec simplify_pgrm (p:c_program) = 
  {globals: fst p ;prototypes: simplify_prototypes snd p; functions:
    simplify_funcs snd p  }
(*
receive: ( global_vars, functions ) functions: c_name, ret_type, c_formals, c_locals, c_body

return to output.ml file: { global_vars, function_prototypes, functions }

function_prototypes:
for each function:
assemble function prototype
add it to list of prototypes

functions:
simplify_locals:
wrap local vars into simple_statement type
simplify_body:
for each c_statement:
match it with c_stmt
if conditional_type: do conditional checks
if loop_type: do loops
if return: simplify_expr, return
if expression: simplify_expr, return
simplify_return:
generate the return type
append return to the end of body
build out simple_func_type to return: { header, args, code }

simplify_expr:
match c_expr:
if StrLiteral: create temp_var and build new expr with temp_var
if floatliteral: ''
if charliteral: ''
if intliteral: '' 
if boolliteral:  ''
if nullliteral: ''
if Id:
if Binop:
if Unop:
if Tree:
if Assign: build out assign using new types, handling child_assign, data_assign,name_assign
if Call:
if NoExpr:
*)
