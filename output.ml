(* 
 * Authors:
 * Chris D'Angelo
 * Kira Whithouse
 * Special thanks to Dara Hazeghi's strlang and Stephen Edward's MicroC
 * which provided background knowledge.
 *)

open Ast
open Check
open Intermediate

let c_of_var_type = function
	  Lrx_Atom(Lrx_Int) -> "int"
	| Lrx_Atom(Lrx_Float) -> "float"
	| Lrx_Atom(Lrx_Bool) -> "bool"
	| Lrx_Atom(Lrx_Char) -> "char"
 	| Lrx_Tree(t) -> "tree *"

let c_of_var_def (v:scope_var_decl) = 
	let (_ ,t, _) = v in match t with
	  Lrx_Atom(Lrx_Int) -> "0"
	| Lrx_Atom(Lrx_Float) -> "0.0"
	| Lrx_Atom(Lrx_Bool) -> "false"
	| Lrx_Atom(Lrx_Char) -> "\'\\0\'"
	| Lrx_Tree(l) -> "lrx_declare_tree(_" ^ String.uppercase (string_of_atom_type l.datatype) ^ "_, " ^ string_of_expr l.degree ^ ")"

let c_of_var_decl (v:scope_var_decl) =
	let (n,t,s) = v in 
	 c_of_var_type t ^ " " ^ n ^ "_" ^ string_of_int s

let c_of_ptr_decl (v:scope_var_decl) =
	let (n,t,s) = v in 
	 c_of_var_type t ^ " *" ^ n ^ "_" ^ string_of_int s

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
		Lrx_Atom(Lrx_Int) -> "fprintf(stdout, \"%d\", " ^ name ^ ")"  
	  | Lrx_Atom(Lrx_Float) -> "fprintf(stdout, \"%f\", " ^ name ^ ")"
	  | Lrx_Atom(Lrx_Char) -> "fprintf(stdout, \"%c\", " ^ name ^ ")"
	  | Lrx_Atom(Lrx_Bool) -> "lrx_print_bool(" ^ name ^ ")"
	  | Lrx_Tree(l) -> "lrx_print_tree(" ^ name ^ ")")

let c_of_print_call = function
	  [] -> ""
	| print_args -> String.concat (";\n") (List.map c_of_print_var print_args)

let unescape_char c =
	match c with 
	   '\n' -> "\\n"
	 | '\t' -> "\\t"
	 | '\"' -> "\\\""
	 | '\\' -> "\\\\"
	 | _ -> String.make 1 c

let c_of_tree_comparator = function
	 Greater -> "_GT_"
   | Less -> "_LT_"
   | Leq -> "_LTE_"
   | Geq -> "_GTE_"
   | Equal -> "_EQ_"
   | Neq -> "_NEQ_"
   | _ -> raise (Failure "Not a valid tree comparator")

let rec c_of_expr = function
  	  Ir_Int_Literal(v, i) -> c_of_var_name v ^ " = " ^ string_of_int i
  	| Ir_Float_Literal(v, f) ->  c_of_var_name v ^ " = " ^ string_of_float f
  	| Ir_String_Literal(v, s) -> c_of_var_name v ^ " = " ^ s (* unescape not required for string. lexer stores raw chars *)
  	| Ir_Char_Literal(v, c) -> c_of_var_name v ^ " = " ^ "\'" ^ unescape_char c ^ "\'"
  	| Ir_Bool_Literal(v, b) -> c_of_var_name v ^ " = " ^ string_of_bool b
  	| Ir_Unop(v1, op, v2) -> c_of_var_name v1 ^ " = " ^ c_of_var_name v2 ^ string_of_unop op
  	| Ir_Binop(v1, op, v2, v3) -> 
  	  let (_,t1,_) = v2 in
      let (_,t2,_) = v3 in
      (match (t1, t2) with
       	  (Lrx_Tree(_), Lrx_Tree(_)) ->
      	  (match op with
      	     (Less | Leq | Greater | Geq | Equal | Neq ) -> 
      	     c_of_var_name v1 ^ " = " ^ "lrx_compare_tree(" ^ c_of_var_name v2 ^ ", " ^ c_of_var_name v3 ^ ", " ^ c_of_tree_comparator op ^ ")"
       	   | Add -> c_of_var_name v1 ^ " = " ^ "lrx_add_trees(" ^ c_of_var_name v2 ^ ", " ^ c_of_var_name v3 ^ ")"
      	   | _ -> raise (Failure "Operation not available between two tree types."))
      	| (Lrx_Atom(_), Lrx_Atom(_)) -> c_of_var_name v1 ^ " = " ^ c_of_var_name v2 ^ " " ^ string_of_binop op ^ " " ^ c_of_var_name v3
      	| _ -> raise (Failure "TEMP need to think what case this is"))
  	| Ir_Id(v1, v2) -> c_of_var_name v1 ^ " = " ^ c_of_var_name v2
  	| Ir_Assign(v1, v2) -> 
  	  let (_,t1,_) = v1 in 
      let (_,t2,_) = v2 in 
  	  (match (t1, t2) with 
  		  (Lrx_Tree(_), Lrx_Tree(_)) -> "lrx_assign_tree_direct(&" ^ c_of_var_name v1 ^ ", &" ^ c_of_var_name v2 ^ ")"
  		| (Lrx_Atom(_), Lrx_Atom(_)) -> c_of_var_name v1 ^ " = " ^ c_of_var_name v2
  		| _ -> raise (Failure "Tree cannot be assigned to atom type."))
  	| Ir_Tree_Literal(v, root, children) -> "lrx_define_tree(" ^ c_of_var_name v ^ ", " ^
  	 	c_of_var_name root ^ ", " ^ c_of_var_name children ^ ")"
	| Ir_Call(v1, v2, vl) ->
		if (fst_of_four v2) = "print" then (c_of_print_call vl)
		else c_of_var_name v1 ^ " = " ^ fst_of_four v2 ^ "( " ^ c_of_func_decl_args vl ^ " )"

let c_of_ref (r:scope_var_decl) =
	let (n ,t, s) = r in 
	"&" ^ n ^ "_" ^ string_of_int s

let rec c_of_leaf (n:string) (d:int) = 
	if d < 0 then "" else
	n ^ "[" ^ string_of_int d ^ "] = NULL; /* c_of_leaf */\n" ^ c_of_leaf n (d - 1)

let c_of_stmt (v:ir_stmt) =
	match v with 
	   Ir_Decl(d) -> c_of_var_decl d ^ " = " ^ c_of_var_def d ^ "; /* Ir_Decl */"
	 | Ir_Leaf(p, d) -> c_of_var_decl p ^ "[" ^ string_of_int d ^ "]; /* Ir_Leaf */\n" ^
	   c_of_leaf (c_of_var_name p) (d - 1) 
     | Ir_Child_Array(d, s) -> c_of_var_decl d ^ "[" ^ string_of_int s ^ "]; /* Ir_Child_Array */"
	 | Ir_Internal(a, c, t) -> c_of_var_name a ^ "[" ^ string_of_int c ^ "] = " ^ c_of_var_name t ^ "; /* Ir_Internal */"
	 | Ir_Ptr(p, r) -> c_of_ptr_decl p ^ " = " ^ c_of_ref r ^ "; /* Ir_Ptr */"
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