


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
	"#include <stdio.h>\n\n int main() { printf(\"dummy program\\n\"); return 0; }"