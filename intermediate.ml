open Ast
open Check

(* 

type inter_stmt =
    Ir_CodeBlock of inter_block
  | Ir_Expr of inter_expr
  | Ir_Return of inter_expr
  | Ir_If of inter_expr * inter_block * inter_block
  | Ir_For of inter_expr * inter_expr * inter_expr * inter_block
  | Ir_While of inter_expr * inter_block
  | Ir_Continue
  | Ir_Break


and inter_block = {
    ir_locals: inter_var list;
    ir_statements: inter_stmt list;
    ir_block_id: int;
}

and inter_func = {
  ir_name: string;
  ir_ret_type: inter_var_type;
  ir_formals: inter_var list;
  ir_block: inter_block;
} *)


let tmp_reg_id = ref 0
let label_id = ref 0

let gen_tmp_var t =
  let x = tmp_reg_id.contents in 
  let prefix = 
  (match t with
      Lrx_Atom(Lrx_Bool) -> "__tmp_bool_" 
    | Lrx_Atom(Lrx_Char) -> "__tmp_char_"
    | Lrx_Atom(Lrx_Int) -> "__tmp_int_"
    | Lrx_Atom(Lrx_Float) -> "__tmp_float_"
    | _ -> raise(Failure("unsupported type"))) in
  tmp_reg_id := x + 1; (prefix, t, x)

let gen_tmp_label (s:unit) =
  let x = label_id.contents in
  label_id := x + 1; "__LABEL_" ^ (string_of_int x)

type ir_expr =
    Ir_Int_Literal of var_type * string * int
  | Ir_Float_Literal of var_type * string * float
  | Ir_String_Literal of var_type * string * string
  | Ir_Char_Literal of var_type * string * char
  | Ir_Bool_Literal of var_type * string * bool

(*   | Ir_Null_Literal
  | Ir_Id of inter_var_type * string
  | Ir_Binop of inter_var_type * inter_expr * op * inter_expr
  | Ir_Unop of inter_var_type * inter_expr * uop
  | Ir_Tree of inter_var_type * int * inter_expr * inter_expr list
  | Ir_Assign of inter_var_type * inter_expr * inter_expr
  | Ir_Call of inter_func_decl * inter_expr list
  | Ir_Noexpr *)

type ir_stmt =
  (* | If of simple_var * string
  | Jmp of string
  | Label of string *)
  | Ir_Decl of scope_var_decl
  | Ir_Ret of scope_var_decl
  | Ir_Expr of ir_expr

type ir_func = {
  ir_header: var_type * string * scope_var_decl list;
  ir_vdecls: scope_var_decl list;
  ir_stmts: ir_stmt list;
}

type ir_fheader = {
  ir_name: string;
  ir_ret_type: var_type;
  ir_formals: var_type list;
}

type ir_program = {
    ir_globals: scope_var_decl list;
    ir_headers: ir_fheader list;
    ir_bodies: ir_func list;
}



(*


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


(* let gen_ir_id x (id:int) =
  let prefix = 
    match x with
      Lrx_Tree(t, d) -> ("__ir_tree_" ^ string_of_var_type t ^ "_" ^ string_of_int d ^ "_")
      | Lrx_Atom(Lrx_Bool) -> "__ir_bool_" 
      | Lrx_Atom(Lrx_Char) -> "__ir_char_"
      | Lrx_Atom(Lrx_Int) -> "__ir_int_"
      | Lrx_Atom(Lrx_Float) -> "__ir_float_"
      | _ -> raise(Failure("attempting to generate ir id for unsupported type"))) 
  in (prefix ^ (string_of_int id), x, -1, id + 1)

let rec gen_ir_expr (e:c_expr) (id:int) =
  match e with
       C_Int_Literal(i) -> 
       let (s, v, _, id) = gen_ir_id Lrx_Atom(Lrx_Int) id in
       ([Decl((s,v,_,id))); Expr(Lit((s,v,_,id), IntLit(i)))], (s,v,_,id)) //
     | C_Float_Literal(f) -> 
     | C_String_Literal(s) ->
     | C_Char_Literal(c) -> 
     | C_Bool_Literal(b) -> 
     | C_Tree(t, d, e, el) ->
     | C_Id(t, s) -> 
     | C_Binop(t, e1, op, e2) ->
     | C_Assign(t, l, r) ->
     | C_Unop(t, e, op) ->
     | C_Call(fd, el) -> 
     | C_Null_Literal ->
     | C_Noexpr -> 

let rec gen_ir_statement (s:c_stmt) =
    match s with
       C_CodeBlock(b) ->
     | C_Return(e) ->
     | C_Expr(e) -> 
     | C_If(e, b1, b2) -> 
     | C_For(e1, e2, e3, b) -> 
     | C_While(e, b) -> 
     | C_Continue ->
     | C_Break -> *)

(* let rec gen_ir_func (f:c_func) =
  () *)
(*let ir_block = gen_ir_block f.c_fblock in 
  let ir_formals = gen_ir_expr f.c_formals in
  {f.c_fname, f.c_ret_type, ir_formals, ir_block} *)

(* and gen_ir_fbodys (flist:c_func list) =
  match flist with
  [] -> []
  | head :: tail -> gen_ir_func_header head :: gen_ir_funcs tail *)
*)*)


let gen_ir_default_ret (t: var_type) =
  let tmp = gen_tmp_var t in
    Ir_Decl(tmp) :: [Ir_Ret(tmp)]

(*   Lrx_Atom(Lrx_Int) -> IntLiteral(0)
  | Lrx_Atom(Lrx_Float) -> "0.0"
  | Lrx_Atom(Lrx_Bool) -> "false"
  | Lrx_Atom(Lrx_Char) -> "\'0\'"
  | _ -> raise (Failure ("chris says (k)IMPOSSIBLE!")) *)

let gen_ir_body (f: c_func) =
  let header = (f.c_ret_type, f.c_fname, f.c_formals) in
  let default_ret = gen_ir_default_ret f.c_ret_type in
  {ir_header = header; ir_vdecls = []; ir_stmts = [] @ default_ret}
(* 
let body = simplify_block f.c_body in
  let vdecls = List.filter is_vdecl body in
  let stmts = List.filter is_not_vdecl body in
  args = f.c_formals; code = vdecls @ stmts} *)


let rec gen_ir_fbodys (flist:c_func list) =
  match flist with
  [] -> []
  | head :: tail -> gen_ir_body head :: gen_ir_fbodys tail

and gen_ir_fdecls (flist:c_func list) = 
  match flist with
  [] -> []
  | head :: tail -> 
  {ir_name = head.c_fname; ir_ret_type = head.c_ret_type; ir_formals = List.map snd_of_three head.c_formals} :: gen_ir_fdecls tail

let rec intermediate_rep_program (p:c_program) =
  let ir_fdecls = gen_ir_fdecls (snd p) in 
  let ir_fbodys = gen_ir_fbodys (snd p) in 
  {ir_globals = fst p; ir_headers = ir_fdecls; ir_bodies = ir_fbodys}





