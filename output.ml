(* 

let c_of_op = function
	 Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"
  
let c_of_uop = function
	Neg -> "-" | Not -> "!" | _ -> raise(Failure("internal error"))
	
 *)

open Ast
open Check
open Intermediate

let c_of_var_type = function
	 Lrx_Atom(Lrx_Int) -> "int"
	| Lrx_Atom(Lrx_Float) -> "float"
	| Lrx_Atom(Lrx_Bool) -> "int"
	| Lrx_Atom(Lrx_Char) -> "char"
(* 	| Lrx_Tree -> "struct lrx_tree"
 *)	| _ -> raise(Failure("TEMP"))

let c_of_var_decl (v:scope_var_decl) =
	let (n,t,s) = v in 
	 c_of_var_type t ^ " " ^ n ^ "_" ^ string_of_int s

let c_of_var_decl_list = function
	[] -> "" 
	| vars -> (String.concat (";\n") (List.map c_of_var_decl vars)) ^ ";\n\n"
	
let c_of_func_decl_args = function
	[] -> ""
	| args -> String.concat (", ") (List.map c_of_var_decl args)

let c_of_func (f: ir_func) =
	let (t, n, sl) = f.ir_header in 
	c_of_var_type t ^ " " ^ n ^ "(" ^ c_of_func_decl_args sl ^ ")\n{" ^
	c_of_var_decl_list f.ir_vdecls ^ "}"

let c_of_func_list = function
	[] -> "" 
	| funcs -> String.concat ("\n") (List.map c_of_func funcs)

let c_of_func_decl_formals = function
    [] -> ""
	| formals -> String.concat (", ") (List.map c_of_var_type formals)

let c_of_func_decl (f:ir_fheader) =
	(c_of_var_type f.ir_ret_type) ^ " " ^ f.ir_name ^
	"(" ^ (c_of_func_decl_formals f.ir_formals) ^ ");"

let c_of_func_decl_list = function
	[] -> ""
	| fdecls -> String.concat ("\n") (List.map c_of_func_decl fdecls) ^ "\n\n"


let c_of_inter_pgrm (p:ir_program) =
	"#include \"lrxlib.h\"\n" ^ 
	c_of_var_decl_list p.ir_globals ^
    c_of_func_decl_list p.ir_headers ^
    c_of_func_list p.ir_bodies