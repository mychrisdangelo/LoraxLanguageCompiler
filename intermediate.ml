open Ast
open Check


type inter_var = Ast.var
type inter_var_type = Ast.var_type
type inter_func_decl = Ast.scope_func_decl

type inter_expr =
    Ir_Int_Literal of int
  | Ir_Float_Literal of float
  | Ir_String_Literal of string
  | Ir_Char_Literal of char
  | Ir_Bool_Literal of bool
  | Ir_Null_Literal
  | Ir_Id of inter_var_type * string
  | Ir_Binop of inter_var_type * inter_expr * op * inter_expr
  | Ir_Unop of inter_var_type * inter_expr * uop
  | Ir_Tree of inter_var_type * int * inter_expr * inter_expr list
  | Ir_Assign of inter_var_type * inter_expr * inter_expr
  | Ir_Call of inter_func_decl * inter_expr list
  | Ir_Noexpr

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
}

type inter_pgrm = inter_var list * inter_func list

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
*)

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

let rec gen_ir_func (f:c_func) =
  ()
(*let ir_block = gen_ir_block f.c_fblock in 
  let ir_formals = gen_ir_expr f.c_formals in
  {f.c_fname, f.c_ret_type, ir_formals, ir_block} *)

and gen_ir_funcs (flist:c_func list) =
  match flist with
  [] -> []
  | head :: tail -> gen_ir_func head :: gen_ir_funcs tail

and gen_ir_decls (flist:var list) = 
  match flist with
  [] -> []
  | head :: tail -> (fst head) :: gen_ir_decls tail

let rec intermediate_rep_program (p:c_program) =
  let ir_decls = gen_ir_decls (fst p) in 
  let ir_funcs = gen_ir_funcs (snd p) in 
  (ir_decls, ir_funcs)