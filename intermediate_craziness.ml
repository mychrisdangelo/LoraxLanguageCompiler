open Ast
open Check

(*  type inter_expr =
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
    Ir_CodeBlock of c_block
  | Ir_Expr of inter_expr
  | Ir_Return of inter_expr
  | Ir_If of inter_expr * inter_block * inter_block
  | Ir_For of inter_expr * inter_expr * inter_expr * inter_block
  | Ir_While of inter_expr * inter_block
  | Ir_Continue
  | Ir_Break *)

type ir_fheader = {
  ir_name: string;
  ir_ret_type: var_type;
  ir_formals: var_type list;
}

type ir_program = {
    globals: var list;
    headers: ir_fheader list;
    bodies: c_func list;
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






let ir_block = gen_ir_block f.c_fblock in 
  let ir_formals = gen_ir_expr f.c_formals in
  {f.c_fname, f.c_ret_type, ir_formals, ir_block} *)
*)*)

let tmp_id = ref 0
let label_id = ref 0

let gen_ir_id x =
  let prefix = 
    match x with
(*       Lrx_Tree(t, d) -> ("__ir_tree_" ^ string_of_var_type t ^ "_" ^ string_of_int d ^ "_")
 *)   | Lrx_Atom(Lrx_Bool) -> "__ir_bool_" 
      | Lrx_Atom(Lrx_Char) -> "__ir_char_"
      | Lrx_Atom(Lrx_Int) -> "__ir_int_"
      | Lrx_Atom(Lrx_Float) -> "__ir_float_"
      | _ -> raise(Failure("attempting to generate ir id for unsupported type"))) 
  in (prefix ^ (string_of_int tmp_id), x, tmp_id); tmp_id := tmp_id + 1

let gen_ir_label (s:unit) =
  let x = label_id.contents in
  label_id := x + 1; "__LABEL_" ^ (string_of_int x)

let rec gen_ir_expr (e:c_expr) =
  match e with
       C_Int_Literal(i) ->
       let (s, v) = gen_ir_id Lrx_Atom(Lrx_Int) in
       
       ([Decl((s,v,id))); Expr(Lit((s,v,id), IntLit(i)))], (s,v,id)) //


    let tmp = gen_tmp_var (Simple(Str)) in
    ( [Decl(tmp); 
       Expr(Lit(tmp, StrLit(s)))]
       , tmp)

 (*  | C_Float_Literal(f) -> 
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
     | C_Noexpr ->  *)
     | _ -> "TEMP: gen_ir_expr"

let rec gen_ir_statement (s:c_stmt) =
  match s with
     C_CodeBlock(b) -> gen_ir_block b
     | C_Return(e) -> let (ir_decls, ir_exprs) = gen_ir_expr e in ir_e @ [Ret(ir_r)]
     | C_Expr(e) -> let (ir_e, _) = gen_ir_expr e in ir_e
     | _ -> "unfinished gen_ir_statement"
     (*
     | C_If(e, b1, b2) -> 
     | C_For(e1, e2, e3, b) -> 
     | C_While(e, b) -> 
     | C_Continue ->
     | C_Break -> *)

(*
  CodeBlock(b) -> simplify_block b
  | Conditional(e, b1, b2) ->
    let (se, r) = simplify_expr e in
    let sb1 = simplify_block b1 in
    let sb2 = simplify_block b2 in
    let startlabel = gen_tmp_label () in
    let endlabel = gen_tmp_label () in
    se @ [If(r, startlabel)] @ sb2 @ [Jmp(endlabel); Label(startlabel)] @ sb1 @ [Label(endlabel)]
  | Loop(e, b) ->
    let (se, r) = simplify_expr e in
    let sb = simplify_block b in
    let startlabel = gen_tmp_label () in
    let endlabel = gen_tmp_label () in
    [Jmp(endlabel); Label(startlabel)] @ sb @ [Label(endlabel)] @ se @ [If(r, startlabel)]
  | Return(e) ->
    let (se, r) = simplify_expr e in
    se @ [Ret(r)]
  | Expression(e) -> (* only need simplified statements, not final tmp register *)
    let (se, r) = simplify_expr e in
    se*)

let is_vdecl = function
    (n,t) -> true
    | _ -> false

let is_not_vdecl s = 
    not (is_vdecl s)

let rec gen_ir_statement_list (sl: c_stmt list) (b_id: int) =
    match sl with
    [] -> []
    | head :: tail -> gen_ir_statement head :: gen_ir_statement_list tail

let add_block_id (vl:var list) (b_id:int) =
    List.rev (List.fold_left (fun l e -> (fst e ^ '_' ^ (string_of_int b_id), snd e)::l) [] vl)

let gen_ir_block (b:c_block) =
    let decls = add_block_id b.c_locals b.c_block_id in 
    decls @ (gen_ir_statement_list b.c_statements b.c_block_id)

let gen_ir_body (f:c_func) =
  let flat_body = gen_ir_block f.c_fblock in
  let fall_back_ret = gen_ir_default_ret f.c_ret_type in
  let flat_body_w_ret = flat_body @ fall_back_ret in
  let vdecls = List.filter is_vdecl flat_body_w_ret in
  let stmts = List.filter is_not_vdecl flat_body_w_ret in
  {c_fname = f.c_fname; c_ret_type = f.c_ret_type; c_formals = f.c_formals; c_fblock = vdecls @ stmts}

let rec gen_ir_fbodies (flist:c_func list) =
  match flist with
  [] -> []
  | head :: tail -> gen_ir_fbody head :: gen_ir_fbodies tail

and gen_ir_fdecls (flist:c_func list) = 
  match flist with
  [] -> []
  | head :: tail -> 
  {ir_name = head.c_fname; ir_ret_type = head.c_ret_type; ir_formals = List.map snd head.c_formals} :: gen_ir_fdecls tail

let rec intermediate_rep_program (p:c_program) =
  let ir_fdecls = gen_ir_fdecls (snd p) in 
  let ir_fbodies = gen_ir_fbodies (snd p) in 
  {globals = fst p; headers = ir_fdecls; bodies = ir_fbodies}