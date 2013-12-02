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
 *)	| _ -> raise(Failure("TEMP C of VAR TYPE"))

let c_of_var_def (v:scope_var_decl) = 
	let (_,t, _) = v in match t with
	 Lrx_Atom(Lrx_Int) -> "0"
	| Lrx_Atom(Lrx_Float) -> "0.0"
	| Lrx_Atom(Lrx_Bool) -> "false"
	| Lrx_Atom(Lrx_Char) -> "\'0\'"
	| _ -> raise(Failure ("TEMP C of VAR DECL"))

let c_of_var_decl (v:scope_var_decl) =
	let (n,t,s) = v in 
	 c_of_var_type t ^ " " ^ n ^ "_" ^ string_of_int s

let c_of_var_decl_list = function
	[] -> "" 
	| vars -> (String.concat (";\n") (List.map c_of_var_decl vars)) ^ ";\n\n"
	
let c_of_func_decl_args = function
	[] -> ""
	| args -> String.concat (", ") (List.map c_of_var_decl args)

let c_of_var_name (v:scope_var_decl) = 
	let (n,_,s) = v in 
	 n ^ "_" ^ string_of_int s

let c_of_expr = function
  	Ir_Int_Literal(v, i) -> c_of_var_name v ^ " = " ^ string_of_int i
  	| Ir_Float_Literal(v, f) ->  c_of_var_name v ^ " = " ^ string_of_float f
  	| Ir_String_Literal(v, s) -> c_of_var_name v ^ " = " ^ s
  	| Ir_Char_Literal(v, c) -> c_of_var_name v ^ " = " ^ "\'" ^ String.make 1 c ^ "\'"
  	| Ir_Bool_Literal(v, b) -> c_of_var_name v ^ " = " ^ string_of_bool b
  	| Ir_Unop(v1, op, v2) -> c_of_var_name v1 ^ " = " ^ c_of_var_name v2 ^ string_of_unop op
  	| Ir_Binop(v1, op, v2, v3) -> c_of_var_name v1 ^ " = " ^ c_of_var_name v2 ^ " " ^ string_of_binop op ^ " " ^ c_of_var_name v3 
  	| _ -> (raise (Failure ("TEMP C of EXPR")))

let c_of_stmt (v:ir_stmt) =
	match v with 
	 Ir_Decl(d) -> c_of_var_decl d ^ " = " ^ c_of_var_def d
  	| Ir_Ret(v) -> "return " ^ c_of_var_name v;
   	| Ir_Expr(e) -> c_of_expr e
    | _ -> raise (Failure ("TMP C of STMT"))

let c_of_stmt_list = function
	[] -> ""
	| stmts -> String.concat (";\n") (List.map c_of_stmt stmts) ^ ";\n\n"

let c_of_func (f: ir_func) =
	let (t, n, sl) = f.ir_header in 
	c_of_var_type t ^ " " ^ n ^ "(" ^ c_of_func_decl_args sl ^ ")\n{\n" ^
	c_of_stmt_list f.ir_vdecls ^ c_of_stmt_list f.ir_stmts ^ "}"

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