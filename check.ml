(*
 * Authors:
 * Chris D'Angelo
 * Tim Paine
 * Kira Whithouse
 * Special thanks to Dara Hazeghi's strlang which provided background knowledge.
 *)

open Ast

(*expressions from Ast but with typing added*)
type c_expr =
    Int_Literal of int
  | Float_Literal of float
  | String_Literal of string
  | Char_Literal of char
  | Bool_Literal of bool
  | Null_Literal
  | Id of var_type * string
  | Binop of var_type * c_expr * op * c_expr
  | Unop of var_type * c_expr * uop
  | Tree of var_type c_expr * c_expr list
  | Assign of var_type * c_expr * c_expr
  | Call of func_decl * string * c_expr list
  | NoExpr

(*statements from Ast but with typing added*)
type c_stmt =
    Block of c_stmt list
  | Expr of c_expr
  | Return of c_expr
  | If of c_expr * c_stmt * c_stmt
  | For of c_expr * c_expr * c_expr * c_stmt
  | While of c_expr * c_stmt
  | Continue
  | Break

(*tree declaration from Ast but with typing added*)
type c_tree_decl = {
       datatype: atom_type;
       degree: c_expr;
} 

(*unsure about this one*)
type c_func_decl = {
    fname : string;
    ret_type : var_type;
    formals : var list;
    locals : var list;
    body : c_stmt list;
} 

(*unsure about this one*)
type c_program = var_decl list * func_decl list;

(*builds a function declaration with a name, return type, and variable argument list*)
let build_fdecl (name:string) (ret:var_type) (args:var_type list) =
	(name, ret, args, 0)

(*gets the return type of a function*)
let get_ret_of_fdecl (f:func_decl) =
	let (_,t,_,_) = f in
	t

(*structures the 'main' function*)
let main_fdecl (f:c_func) =
	let fdecl = f.c_header in
	let (name, t, formals, _) = fdecl in
	if name = "main" && t = Lrx_Atom(Lrx_Int) && formals = [] then true
	else false
               
(*called to get the Atom/Tree type of an expresion*)
type type_of_expr =
    Int_Literal(i) -> Lrx_Atom(Lrx_Int)
  | Float_Literal(f) -> Lrx_Atom(Lrx_Float)
  | String_Literal(s) -> (*what do we do for strings?*)
  | Char_Literal(c) -> Lrx_Atom(Lrx_Char)
  | Bool_Literal(b) -> Lrx_Atom(Lrx_Bool) 
  | Null_Literal
  | Id(t,_) -> t (*not sure about this*)
  | Binop(t,_,_,_) -> t 
  | Unop(t,_,_) -> t 
  | Tree(t, d) -> Lrx_Tree(t, d) (*not sure about this*)
  | Assign(t,_,_) -> t 
  | Call(fdecl,_) -> let (_,t,_,_) = fdecl in t
  | Noexpr -> "" (*not sure about this *)

(*error raised for improper binary expression*)
let binop_error (t1:var_type) (t2:var_type) (op:Ast.bop) =
	raise(Failure("operator " ^ (string_of_binop op) ^ " not compatible with expressions of type " ^
		(string_of_type t1) ^ " and " ^ (string_of_type t2)))

(*error raised for improper unary expression*)
let unop_error (t:var_type) (op:Ast.uop) =
	raise(Failure("operator " ^ (string_of_unop op) ^ " not compatible with expression of type " ^
		(string_of_type t)))


(*check binary operators ADD SUB MULT DIV MOD EQUAL NEQ LESS LEQ GREATER GEQ CHILD AND OR*)
let check_binop (c1:c_expr) (c2:c_expr) (op:Ast.bop) =
        let (t1, t2) = (type_of_expr c1, type_of_expr c2) in
        match(t1, t2) with 
                (Lrx_Atom(Lrx_Int), Lrx_Atom(Lrx_Int)) -> (*two integer*)
                        let f = (match op with
                        (Add | Sub | Mult | Div | Mod | Equal | Neq | Less | Leq | Greater | Geq) -> Binop(Lrx_Atom(Lrx_Int), c1, op, c2)
                        | _ -> binop_error t1 t2 op) in
                                FuncCall(f, [c1; c2])
                | (Lrx_Atom(Lrx_Float), Lrx_Atom(Lrx_Float)) -> (*two floats*)
                         let f = (match op with
                        (Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq) -> Binop(Lrx_Atom(Lrx_Float), c1, op, c2)
                        | _ -> binop_error t1 t2 op) in
                                FuncCall(f, [c1; c2])

                | (Lrx_Atom(Lrx_Bool), Lrx_Atom(Lrx_Bool)) -> (*two bools*)
                         let f = (match op with
                        (Equal | Neq | And | Or) -> Binop(Lrx_Atom(Lrx_Bool), c1, op, c2)
                        | _ -> binop_error t1 t2 op) in
                                FuncCall(f, [c1; c2])
                | (Lrx_Atom(Lrx_Char), Lrx_Atom(Lrx_Char)) ->
                         let f = (match op with
                        (Add | Sub |  Equal | Neq | Less | Leq | Greater | Geq) -> Binop(Lrx_Atom(Lrx_Float), c1, op, c2)
                        | _ -> binop_error t1 t2 op) in
                                FuncCall(f, [c1; c2])
                | (Lrx_Tree, Lrx_Tree) -> (*two trees*)
                        let f = (match op with
                        Add -> build_fdecl "__tree_concat" (Lrx_Tree) [t1; t2]
                        | Equal -> build_fdecl "__tree_equal" (Lrx_Atom(Lrx_Bool)) [t1; t2]
                        | Neq -> build_fdecl "__tree_not_equal" (Lrx_Atom(Lrx_Bool)) [t1; t2]
                        | Less -> build_fdecl "__tree_less_than" (Lrx_Atom(Lrx_Bool)) [t1;t2]
                        | Leq -> build_fdecl "__tree_less_equal_than" (Lrx_Atom(Lrx_Bool)) [t1;t2]
                        | Greater -> build_fdecl "__tree_greater_than" (Lrx_Atom(Lrx_Bool)) [t1;t2]
                        | Geq -> build_fdecl "_tree_greater_equal_than" (Lrx_Atom(Lrx_Bool)) [t;t2]
                        | _ -> binop_error t1 t2 op) in
                                FuncCall(f, [c1; c2])
                | (Lrx_Tree, Lrx_Atom(Lrx_Int)) -> (*tree and int --> child operator*) 
                        let f = (match op with
                        Child -> build_fdecl "__tree_child" (Lrx_Tree) [t1; t2]
                        | _ -> binop_error t1 t2 op) in
                                FuncCall(f, [c1; c2])
                 | _ -> binop_error t1 t2 op) in
                                FuncCall(f, [c1; c2])

 (*check unary operators NEG NOT POP AT*) 
let check_unop (c:c_expr) (op:Ast.uop) = 
        let t = type_of_expr c in
        match t with
                Lrx_Atom(Lrx_Int) ->
                        (match op with
                                (Neg | Not) -> Unop((Lrx_Atom(Lrx_Int)), c, op)
                                | _ -> unop_error t op)
                | Lrx_Atom(Lrx_Float) ->
                         (match op with
                                (Neg | Not) -> Unop((Lrx_Atom(Lrx_Float)), c, op)
                                | _ -> unop_error t op)
                | Lrx_Tree (a:atom_type) (d:c_expr) ->
                        let t_d = type_of_expr d in
                        match t_d  with
                                |Lrx_Atom(Lrx_Int) ->
                                         (match op with
                                                (Not -> Unop(Lrx_Tree, c, op)) 
                                                | Pop -> build_function "__tree_pop" () [a;d]
                                                | At -> build_function "__tree_at" () [a;d]
                                                | _ -> unop_error t op)
                                | _ -> unop_error t op (*declaring a tree with degree where degree is not type int *)
               | _ -> unop_error t op       
                        
(*compares argument list*)
let rec compare_arglists formals actuals =
	match (formals,actuals) with
	([],[]) -> true
	| (head1::tail1, head2::tail2)
		-> (head1 = head2) && compare_arglists tail1 tail2
	| _ -> false

(*checks that a function declaration and calling is proper, such that a function is called with the proper number and type of arguments*)
and check_func (name:string) (cl:c_expr list) env =
	let decl = Symtab.symtab_find name env in
	let func = (match decl with FuncDecl(f) -> f
		| _ -> raise(Failure("symbol " ^ name ^ " is not a function"))) in
	let (_,t,formals,_) = func in
	let actuals = List.map type_of_expr cl in
	if (List.length formals) = (List.length actuals) then
		if compare_arglists formals actuals then
			FuncCall(func, cl)
		else
			raise(Failure("function " ^ name ^ "'s argument types don't match its formals"))
	else raise(Failure("function " ^ name ^ " expected " ^ (string_of_int (List.length actuals)) ^
		" arguments but called with " ^ (string_of_int (List.length formals))))

(*checks expression*)
and check_expr (e:expr) env =
	match e with
        Ast.Int_Literal(i) -> Int_Literal(i)
        | Ast.Float_Literal(f) -> Float_Literal(f)
        | Ast.StringLiteral(s) -> String_Literal(s)
        | Ast.Char_Literal(c) -> Char_Literal(c)
        | Ast.Bool_Literal(b) -> Bool_Literal(b)
        | Ast.Null_Literal
        | Ast.Id(s) -> Id(s)
        | Ast.Binop(e1, op, e2) ->
		let (c1, c2) = (check_expr e1 env, check_expr e2 env) in
		check_binop c1 c2 op
	| Ast.Unop(e1, op) ->
		let checked = check_expr e1 env in
		check_unop checked op
        | Ast.Tree(t) ->
                check_tree t
	| Ast.Assign(l, ae) ->
		let checked = check_expr ae env in
		let deref = check_expr (snd l) env in
		let (result_t, lv) = check_lvalue l deref env in
		let t = type_of_expr checked in
		if t = result_t then Assign(t, lv, checked)
		else 
			(match (checked, result_t) with
				(NumLiteral(0), Map(_,_)) -> let f = build_fdecl "__map_empty" result_t [result_t] in
				FuncCall(f, [Rvalue(result_t, lv)])
			| _ -> raise(Failure("assignment not compatible with expressions of type " ^
			string_of_type result_t ^ " and " ^ string_of_type t)))
	| Ast.Call(name, el) ->
		let checked = check_exprlist el env in
		check_func name checked env
	| Ast.NoExpr -> NoExpr
		
and check_exprlist (el:expr list) env =
	match el with
	[] -> []
	| head :: tail -> (check_expr head env) :: (check_exprlist tail env)

(* check a single statement *)
let rec check_stmt (s:stmt) ret_type env =
	match s with
	Ast.Block(b) -> Block(check_block b ret_type env)
        | Ast.Expr(e) -> Expr(check_expr e env)
        | Ast.Return(e) -> let checked = check_expr e env in
                           let t = type_of_expr checked in
                           if t = ret_type then Return(checked) else
                           raise (Failure("function return type " ^ string_of_type t ^ "; type " ^ string_of_type ret_type ^ "expected"))
        | Ast.If(e, s, Block([])) -> let checked = check_expr e env in
                        if type_of_expr checked = Lrx_Atom(Lrx_Bool) then
                        If(checked, check_stmt s ret_type env, Block([])) 
		        else raise(Failure("if statement must evaluate on boolean expression"))
        | Ast.If(e, s1, s2) -> let checked = check_expr e env in
                        if type_of_expr checked = Lrx_Atom(Lrx_bool) then
                        If(checked, check_stmt s1 ret_type env, check_stmt s2 ret_type env)
		        else raise(Failure("if statement must evaluate on boolean expression"))
        | Ast.For(e1, e2, e3, s) -> let c1, c2, c3 = (check_expr e1 env, check_expr e2 env, check_expr e3 env) in
                        if(type_of_expr c1 = Lrx_Atom(Lrx_Bool)) and 
                        if(type_of_expr c2 = Lrx_Atom(Lrx_Bool)) and
                        if(type_of_expr c3 = Lrx_Atom(Lrx_Bool)) then
                        For(c1, c2, c3, check_stmt s ret_type env)
		        else raise(Failure("foor loop must evaluate on boolean expressions"))
	| Ast.While(e, s) -> let checked = check_expr e env in
		        if type_of_expr checked = Lrx_Atom(Lrx_Bool) then 
                        While(checked, check_stmt s ret_type env)
		        else raise(Failure("while loop must evaluate on boolean expression"))
----

(* check a list of statements *)
and check_stmtlist (s:stmt list) (ret_type:var_type) env =
	match s with
	[] -> []
	| head :: tail -> check_stmt head ret_type env :: check_stmtlist tail ret_type env

(*checks a variable declaration*)
and check_vdecllist (v:var list) env =
	match v with
	[] -> []
	| head :: tail ->
		let decl = Symtab.symtab_find (fst head) env in
		match decl with
			FuncDecl(f) -> raise(Failure("symbol is not a variable"))
			| VarDecl(v) -> v :: check_vdecllist tail env

(*checks a function declaration*)
and check_fdecl (f:string) env =
	let decl = Symtab.symtab_find f env in
	match decl with
		VarDecl(v) -> raise(Failure("symbol is not a function"))
		| FuncDecl(f) -> f

(* check a block *)
and check_block (b:block) (ret_type:var_type) env =
	let vars = check_vdecllist b.locals (fst env, b.block_id) in
	{ c_locals = vars;
		c_statements = check_stmtlist b.statements ret_type (fst env, b.block_id);
		c_block_id = b.block_id}

(* check a function *)
and check_func (f:func) env =
	let checked = check_block f.body f.ret_type env in
	let formals = check_vdecllist f.formals (fst env, f.body.block_id) in
	{ c_header = check_fdecl f.name env; c_body = checked; c_formals = formals }

(* check a function list *)
and check_funclist (funcs:func list) env =
	match funcs with
	[] -> []
	| head :: tail -> check_func head env :: check_funclist tail env

and check_main (f:c_func list) =
	if (List.filter main_fdecl f) = [] then false
	else true

let check_program (p:program) env =
	let vars = check_vdecllist p.globals env in
	let checked = check_funclist p.functions env in
	if (check_main checked) then {c_globals = vars; c_functions = checked; c_block_count = p.block_count}
	else raise (Failure("function main(^) -> # not found"))
