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
	| Lrx_Atom(Lrx_Bool) -> "bool"
	| Lrx_Atom(Lrx_Char) -> "char"
 	| Lrx_Tree(t) -> "struct *lrx_tree"

let c_of_var_def (v:scope_var_decl) = 
	let (_ ,t, _) = v in match t with
	 Lrx_Atom(Lrx_Int) -> "0"
	| Lrx_Atom(Lrx_Float) -> "0.0"
	| Lrx_Atom(Lrx_Bool) -> "false"
	| Lrx_Atom(Lrx_Char) -> "\'\\0\'"
	| Lrx_Tree(l) -> "construct_tree(" ^ string_of_expr l.degree ^ " , " ^ String.uppercase (string_of_atom_type l.datatype) ^ ")"

let c_of_var_decl (v:scope_var_decl) =
	let (n,t,s) = v in 
	 c_of_var_type t ^ " " ^ n ^ "_" ^ string_of_int s

let c_of_var_decl_list = function
	[] -> "" 
	| vars -> (String.concat (";\n") (List.map c_of_var_decl vars)) ^ ";\n\n"
	
let c_of_func_actual (v:scope_var_decl) =
	let(n,t,s) = v in 
	n ^ "_" ^ string_of_int s	

let c_of_func_decl_args = function
	[] -> ""
	| args -> String.concat (", ") (List.map c_of_func_actual args)

let c_of_func_def_formals = function
	[] -> ""
	| args -> String.concat (", ") (List.map c_of_var_decl args)

let c_of_var_name (v:scope_var_decl) = 
	let (n,_,s) = v in 
	 n ^ "_" ^ string_of_int s

let c_of_print_var (arg :scope_var_decl) =
	let (n ,t, s) = arg in 
	let name = n ^ "_" ^ string_of_int s in
	(match t with
		Lrx_Atom(Lrx_Int) -> "fprintf(stderr, \"%d\", " ^ name ^ ")"  
	  | Lrx_Atom(Lrx_Float) -> "fprintf(stderr, \"%f\", " ^ name ^ ")"
	  | Lrx_Atom(Lrx_Char) -> "fprintf(stderr, \"%c\", " ^ name ^ ")"
	  | Lrx_Atom(Lrx_Bool) -> "lrx_print_bool(" ^ name ^ ")"
	  | Lrx_Tree(l) -> "lrx_print_tree(" ^ name ^ ")")

let c_of_print_call = function
	 [] -> ""
	| print_args -> String.concat (";\n") (List.map c_of_print_var print_args)
 
let rec c_of_expr = function
  	Ir_Int_Literal(v, i) -> c_of_var_name v ^ " = " ^ string_of_int i
  	| Ir_Float_Literal(v, f) ->  c_of_var_name v ^ " = " ^ string_of_float f
  	| Ir_String_Literal(v, s) -> c_of_var_name v ^ " = " ^ s
  	| Ir_Char_Literal(v, c) -> c_of_var_name v ^ " = " ^ "\'" ^ String.make 1 c ^ "\'"
  	| Ir_Bool_Literal(v, b) -> c_of_var_name v ^ " = " ^ string_of_bool b
  	| Ir_Unop(v1, op, v2) -> c_of_var_name v1 ^ " = " ^ c_of_var_name v2 ^ string_of_unop op
  	| Ir_Binop(v1, op, v2, v3) -> c_of_var_name v1 ^ " = " ^ c_of_var_name v2 ^ " " ^ string_of_binop op ^ " " ^ c_of_var_name v3 
  	| Ir_Id(v1, v2) -> c_of_var_name v1 ^ " = " ^ c_of_var_name v2
  	| Ir_Assign(v1, v2) -> c_of_var_name v1 ^ " = " ^ c_of_var_name v2
  	| Ir_Tree_Literal(v, t, i, d, dl) -> c_of_var_name v ^ " =  __generate_tree_literal(" ^
  	 	string_of_var_type t ^ ", " ^ string_of_int i ^ ", " ^ c_of_var_name d ^ "," ^ 
  	 	(String.concat (",") (List.map c_of_var_name dl)) ^ ")"
	| Ir_Call(v1, v2, vl) ->
		if (fst_of_four v2) = "print" then (c_of_print_call vl)
		else c_of_var_name v1 ^ " = " ^ fst_of_four v2 ^ "( " ^ c_of_func_decl_args vl ^ " )"

let c_of_stmt (v:ir_stmt) =
	match v with 
	 Ir_Decl(d) -> c_of_var_decl d ^ " = " ^ c_of_var_def d ^ ";"
  	| Ir_Ret(v) -> "return " ^ c_of_var_name v ^ ";"
   	| Ir_Expr(e) -> c_of_expr e ^ ";\n"
   	| Ir_If(v, s) -> "if(" ^ c_of_var_name v ^ ") goto " ^ s ^ "" ^ ";"
   	| Ir_Jmp(s) -> "goto " ^ s ^ ";"
   	| Ir_Label(s) -> s ^ ":"

let c_of_stmt_list = function
	[] -> ""
	| stmts -> String.concat ("\n") (List.map c_of_stmt stmts) ^ "\n\n"

let c_of_func (f: ir_func) =
	let (t, n, sl) = f.ir_header in 
	c_of_var_type t ^ " " ^ n ^ "(" ^ c_of_func_def_formals sl ^ ")\n{\n" ^
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