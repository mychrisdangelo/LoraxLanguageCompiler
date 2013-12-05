open Ast
open Check

let tmp_reg_id = ref 0
let label_id = ref 0

let string_of_tmp_var_type  = function
    Lrx_Atom(t) -> string_of_atom_type t
  | Lrx_Tree(t) -> "tree_datatype_" ^ string_of_atom_type t.datatype ^ "_degree_" ^ string_of_expr t.degree 

let gen_tmp_var t =
  let x = tmp_reg_id.contents in 
  let prefix = "__tmp_" ^ string_of_tmp_var_type t in 
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
  | Ir_Id of scope_var_decl * scope_var_decl
  | Ir_Assign of scope_var_decl * scope_var_decl
  | Ir_Tree_Literal of scope_var_decl * var_type * int * scope_var_decl * scope_var_decl list (* 4[3, 2[]]*)
  | Ir_Call of scope_var_decl * scope_func_decl * scope_var_decl list
(*
  | Ir_Null_Literal
  | Ir_Noexpr *)

type ir_stmt =
  | Ir_If of scope_var_decl * string
  | Ir_Jmp of string
  | Ir_Label of string
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
       let t = type_of_expr e in  
       (match t with 
           Lrx_Tree(_) -> raise (Failure ("TEMP unop not implemented for tree pop/at"))
         | _ -> let tmp = gen_tmp_var v in
          ([Ir_Decl(tmp)] @ s @ [Ir_Expr(Ir_Unop(tmp, o, r))], tmp))
  | C_Binop(v, e1, o, e2) -> 
      let (s1, r1) = gen_ir_expr e1 in
      let (s2, r2) = gen_ir_expr e2 in
      let tmp = gen_tmp_var v in
      (* check if binop contains tree on lhs *)
      let t1 = type_of_expr e1 in
        (match t1 with 
            Lrx_Tree(_) -> raise (Failure("TEMP binop tree"))
          | _ ->([Ir_Decl(tmp)] @ s1 @ s2 @ [Ir_Expr(Ir_Binop(tmp, o, r1, r2))], tmp))
  | C_Id(t, s, i) ->
      (* let tmp = gen_tmp_var t in 
       ([Ir_Decl(tmp); Ir_Expr(Ir_Id(tmp, (s, t, i)))], tmp) *)
      ([], (s, t, i))
  | C_Assign(t, l, r) ->
      let (s1, r1) = gen_ir_expr l in
      let (s2, r2) = gen_ir_expr r in
      (s2 @ [Ir_Expr(Ir_Assign(r1, r2))], r2)
  | C_Tree(t, d, e, el) -> 
      let (s, r) = gen_ir_expr e in
      let ir_el = List.map gen_ir_expr el in
      let (sl, rl) = (List.fold_left (fun (sl_ir, rl_ir) (s_ir, r_ir) -> (sl_ir @ s_ir, rl_ir@[r_ir])) ([],[]) ir_el) in 
      let i  =
      (match t with
      Lrx_Atom(a) -> a
      |Lrx_Tree(t) -> raise(Failure("TEMP TREE INSIDE TREE ACTION ???"))) in 
      let tmp = gen_tmp_var (Lrx_Tree({datatype = i; degree = Int_Literal(d)})) in
      (Ir_Decl(tmp) :: s @ sl @ [Ir_Expr(Ir_Tree_Literal(tmp, t, d, r, rl))], tmp)
  | C_Call(fd, el) ->
      let (n, rt, args, s) = fd in 
      let tmp = gen_tmp_var rt in
      let ir_el = List.map gen_ir_expr el in 
      let (sl, rl) = (List.fold_left (fun (sl_ir, rl_ir) (s_ir, r_ir) -> (sl_ir @ s_ir, rl_ir@[r_ir])) ([],[]) ir_el) in 
      (Ir_Decl(tmp) :: sl @ [Ir_Expr(Ir_Call(tmp, fd, rl))], tmp)
  | _ -> raise (Failure ("TEMP gen_ir_expr"))

 (*
     | C_String_Literal(s) ->
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
     | C_If(e, b1, b2) -> 
          let (s, r) = gen_ir_expr e in 
          let irb1 = gen_ir_block b1 in
          let irb2 = gen_ir_block b2 in
          let startlabel = gen_tmp_label () in
          let endlabel = gen_tmp_label () in
          s @ [Ir_If(r, startlabel)] @ irb2 @ [Ir_Jmp(endlabel); Ir_Label(startlabel)] @ irb1 @ [Ir_Label(endlabel)]
     | C_For(e1, e2, e3, b) -> 
        let (s1, r1) = gen_ir_expr e1 in 
        let (s2, r2) = gen_ir_expr e2 in 
        let (s3, r3) = gen_ir_expr e3 in 
        let irb = gen_ir_block b in
        let startlabel = gen_tmp_label () in
        let endlabel = gen_tmp_label () in
        s1 @ [Ir_Jmp(endlabel); Ir_Label(startlabel)] @ s3 @ irb @ [Ir_Label(endlabel)] @ s2 @ [Ir_If(r2, startlabel)]
     | C_While(e, b) -> 
        let (s, r) = gen_ir_expr e in 
        let irb = gen_ir_block b in
        let startlabel = gen_tmp_label () in
        let endlabel = gen_tmp_label () in
        [Ir_Jmp(endlabel); Ir_Label(startlabel)] @ irb @ [Ir_Label(endlabel)] @ s @ [Ir_If(r, startlabel)]
     | _ ->raise (Failure ("TEMP gen_ir_stmt"))
(*      
     
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





