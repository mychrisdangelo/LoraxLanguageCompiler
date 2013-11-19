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
 | ShallowAssign of inter_var * inter_var * inter_var (* this is most likely
 incorrect *)
 | DeepAssign of inter_var * inter_var * inter_var (* this is also incorrect*)

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
  (* TODO *)

and simplify_body (l:c_stmt list) = function 
  (* TODO *)

and simplify_locals (l:var list) = function
  [] -> []
  | _ -> List.map (fun e -> Decl(e)) l 
   
  
and simplify_ret (v:var_type) = function
  (* TODO *)

and simplify_funcs (l:c_func_decl list) = function
  [] -> []
  | head::tail -> simplify_funcs head:: simplify_funcs tail

and simplify_func (f:c_func_decl) = function
  let body = simplify_body f.body in
  let locals = simplify_locals f.locals in
  let ret = simplify_ret f.ret_type in
  let head = simplify_prototype f in
  let formals = f.formals in
  {args = formals; header = head; code: locals @ body @ ret}


and simplify_prototypes (l:c_func_decl list) = function
  [] -> []
  | head::tail -> simplify_prototype head:: simplify_prototypes tail

and simplify_prototype (f:c_func_decl) = function
  {name = f.fname; ret_type = f.ret_type; formals = f.formals}

let rec simplify_pgrm (p:c_program) = 
  {globals: fst p ;prototypes: simplify_prototypes snd p; functions:
    simplify_funcs snd p  }
