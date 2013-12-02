open Ast
open Check

let tmp_reg_id = ref 0
let label_id = ref 0

let gen_tmp_var t =
  let x = tmp_reg_id.contents in 
  let prefix = 
  (match t with
      Lrx_Atom(Lrx_Bool) -> "__tmp_bool" 
    | Lrx_Atom(Lrx_Char) -> "__tmp_char"
    | Lrx_Atom(Lrx_Int) -> "__tmp_int"
    | Lrx_Atom(Lrx_Float) -> "__tmp_float"
    | _ -> raise(Failure("unsupported type"))) in
  tmp_reg_id := x + 1; (prefix, t, x)

let gen_tmp_label (s:unit) =
  let x = label_id.contents in
  label_id := x + 1; "__LABEL_" ^ (string_of_int x)

type ir_expr =
    Ir_Int_Literal of scope_var_decl * int
  | Ir_Float_Literal of scope_var_decl * float
  | Ir_String_Literal of scope_var_decl * string
  | Ir_Char_Literal of scope_var_decl * char
  | Ir_Bool_Literal of scope_var_decl * bool
  | Ir_Unop of scope_var_decl * uop * scope_var_decl
  | Ir_Binop of scope_var_decl * op * scope_var_decl * scope_var_decl
(*| Ir_Null_Literal
  | Ir_Id of inter_var_type * string
  | Ir_Tree of inter_var_type * int * inter_expr * inter_expr list
  | Ir_Assign of inter_var_type * inter_expr * inter_expr
  | Ir_Call of inter_func_decl * inter_expr list
  | Ir_Noexpr *)

type ir_stmt =
  (* | If of simple_var * string
  | Ir_Jmp of string
  | Ir_Label of string *)
  | Ir_Decl of scope_var_decl
  | Ir_Ret of scope_var_decl
  | Ir_Expr of ir_expr

type ir_func = {
  ir_header: var_type * string * scope_var_decl list;
  ir_vdecls: ir_stmt list;
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

let is_decl (s: ir_stmt) =
  match s with
  Ir_Decl(_) -> true
  | _ -> false

let is_not_decl (s:ir_stmt) =
  not (is_decl s)

let gen_ir_default_ret (t: var_type) =
  let tmp = gen_tmp_var t in
    Ir_Decl(tmp) :: [Ir_Ret(tmp)]
  
let rec gen_ir_expr (e:c_expr) =
  match e with
  C_Int_Literal(i) ->
      let tmp = gen_tmp_var (Lrx_Atom(Lrx_Int)) in
      ([Ir_Decl(tmp); Ir_Expr(Ir_Int_Literal(tmp, i))], tmp)
  | C_Float_Literal(f) -> 
      let tmp = gen_tmp_var (Lrx_Atom(Lrx_Float)) in
      ([Ir_Decl(tmp); Ir_Expr(Ir_Float_Literal(tmp, f))], tmp)
  | C_Char_Literal(c) ->
       let tmp = gen_tmp_var (Lrx_Atom(Lrx_Char)) in
      ([Ir_Decl(tmp); Ir_Expr(Ir_Char_Literal(tmp, c))], tmp) 
  | C_Bool_Literal(b) -> 
       let tmp = gen_tmp_var (Lrx_Atom(Lrx_Bool)) in
      ([Ir_Decl(tmp); Ir_Expr(Ir_Bool_Literal(tmp, b))], tmp)
  | C_Unop(v, e, o) ->
       let (s, r) = gen_ir_expr e in 
       let tmp = gen_tmp_var v in
       ([Ir_Decl(tmp)] @ s @ [Ir_Expr(Ir_Unop(tmp, o, r))], tmp)   
  | C_Binop(v, e1, o, e2) -> 
      let (s1, r1) = gen_ir_expr e1 in
      let (s2, r2) = gen_ir_expr e2 in
      let tmp = gen_tmp_var v in
      ([Ir_Decl(tmp)] @ s1 @ s2 @ [Ir_Expr(Ir_Binop(tmp, o, r1, r2))], tmp)
  | _ -> raise (Failure ("TEMP gen_ir_expr"))

 (*
     | C_String_Literal(s) ->
     | C_Tree(t, d, e, el) ->
     | C_Id(t, s) -> 
     | C_Assign(t, l, r) ->
     | C_Call(fd, el) -> 
     | C_Null_Literal ->
     | C_Noexpr -> 
 *)

let rec gen_ir_block (b: c_block) =
  let decls = List.map (fun e -> Ir_Decl(e)) b.c_locals in
  decls @ (gen_ir_stmtlist b.c_statements)

and gen_ir_stmt (s: c_stmt) =
     match s with
       C_CodeBlock(b) -> gen_ir_block b
     | C_Return(e) -> let (s, r) = gen_ir_expr e in s @ [Ir_Ret(r)]
     | C_Expr(e) -> fst (gen_ir_expr e)
(*      | C_If(e, b1, b2) -> 
     | C_For(e1, e2, e3, b) -> 
     | C_While(e, b) -> 
     | C_Continue ->
     | C_Break ->  *)

and gen_ir_stmtlist (slist: c_stmt list) = 
  match slist with
  [] -> []
  | head :: tail -> gen_ir_stmt head @ gen_ir_stmtlist tail

and gen_ir_body (f: c_func) =
  let header = (f.c_ret_type, f.c_fname, f.c_formals) in
  let default_ret = gen_ir_default_ret f.c_ret_type in
  let body = gen_ir_block f.c_fblock @ default_ret in
  let decls = List.filter is_decl body in
  let stmts = List.filter is_not_decl body in
  {ir_header = header; ir_vdecls = decls; ir_stmts = stmts}

and gen_ir_fbodys (flist:c_func list) =
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





