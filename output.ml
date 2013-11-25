(* let c_of_var_type = function
	 Lrx_Atom(Lrx_Int) -> "int"
	| Lrx_Atom(Lrx_Float) -> "float"
	| Lrx_Atom(Lrx_Bool) -> "char"
	| Lrx_Atom(Lrx_Char) -> "char"
	| Lrx_Tree -> "struct lrx_tree"
	| _ -> raise(Failure("internal error"))

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
	
let c_of_var_init = function
	Lorax_Atom(Lrx_int) -> "0"
	| Lorax_Atom(Lrx_float) -> "0.0"
	| Lorax_Atom(Lrx_bool) -> "0"
	| Lorax_Atom(Lrx_char) -> "0"
	| _ -> raise(Failure("internal error")) *)


(* 
 * To: Doug/Zhaarn/Tim
 * From: Chris
 * Message: this is the entry point function that I'm using in 
 * lorax.ml. The input value will NOT be a string but instead
 * will be (p:Intermediate.inter_pgrm)
 *)

open Ast
open Check
open Intermediate

let rec c_of_inter_pgrm (p:string) =
	"#include \"lrxlib.h\"\n" ^
	"#include <stdio.h>\n\nint main() { printf(\"dummy program\\n\"); return 0; }"