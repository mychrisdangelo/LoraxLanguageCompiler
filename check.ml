<<<<<<< HEAD
(* checking pass
 	-perform semantic checks on all expressions
	-annotate expressions in AST with type information
	-resolve identifier names
*)

open Ast

(* checked expression *)
type expr =
	Char_Literal of char
	| Int_Literal of int
	| Float_Literal of float
	| Bool_Literal of bool
	| Null_Literal
  	| Id of string
	| Binop of var_type * expr * bop * expr
	| Unop of var_type * expr * uop
	| Assign of var_type * lvalue * expr (* lvalue and right side *)
	| FuncCall of func_decl * expr list
  	| Tree of expr * expr list
	| Rvalue of var_type * lvalue (* variable (on the right side) *)
	| NoExpr

and lvalue = var_decl * expr

type stmt =
	Block of block
	(*| Conditional of expr * block * block*)
	| Expr of expr
	| Return of expr
  	| If of expr * stmt * stmt
  	| For of expr * expr * expr * stmt
  	| While of expr * stmt	
	| Continue
	| Break

and block = {
	locals: var_decl list;
	statements: stmt list;
	block_id: int;
}

and func = {
	formals: var_decl list;
	header: func_decl;
	body: block;
}

type program = {
	globals: var_decl list;
	functions: func list;
	block_count: int
}

let build_fdecl (name:string) (ret:var_type) (args:var_type list) =
	(name, ret, args, 0)

=======
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
>>>>>>> 88ff4fd8c132a861403f5a599f8f22edced1f3e9
let get_ret_of_fdecl (f:func_decl) =
	let (_,t,_,_) = f in
	t

<<<<<<< HEAD
let main_fdecl (f:func) =
	let fdecl = f.header in
	let (name, t, formals, _) = fdecl in
	if name = "main" && t = Simple(Int) && formals = [] then true
	else false

let type_of_expr = function
	Char_Literal(s) -> Simple(Char)
	| Int_Literal(n) -> Simple(Int)
	| Floar_Literal(n) -> Simple(Float)
	| Bool_Literal(n) -> Simple(Bool)
	| NoExpr -> Simple(None)
	| Binop(t,_,_,_) -> t
	| Unop(t,_,_) -> t
	| Rvalue(t,_) -> t
	| Assign(t,_,_) -> t
	| FuncCall(fdecl,_) -> let (_,t,_,_) = fdecl in t



(* Checking functions *)

(* Binary op used incorrectly *)
=======
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
>>>>>>> 88ff4fd8c132a861403f5a599f8f22edced1f3e9
let binop_error (t1:var_type) (t2:var_type) (op:Ast.bop) =
	raise(Failure("operator " ^ (string_of_binop op) ^ " not compatible with expressions of type " ^
		(string_of_type t1) ^ " and " ^ (string_of_type t2)))

<<<<<<< HEAD
(* Unary op used incorrectly *)
=======
(*error raised for improper unary expression*)
>>>>>>> 88ff4fd8c132a861403f5a599f8f22edced1f3e9
let unop_error (t:var_type) (op:Ast.uop) =
	raise(Failure("operator " ^ (string_of_unop op) ^ " not compatible with expression of type " ^
		(string_of_type t)))

<<<<<<< HEAD
(* Validate the binary op *)
let check_binop (c1:expr) (c2:expr) (op:Ast.bop) =
	let (t1, t2) = (type_of_expr c1, type_of_expr c2) in
let check_binop (c1:expr) (c2:expr) (op:Ast.bop) =
		(Simple(Int), Simple(Int)) -> Binop(Simple(Int), c1, op, c2) (* standard arithmetic binops *)
		| (Simple(Char), Simple(Char)) -> (* string binops with 2 string operands *)
			let f = (match op with
			Less -> build_fdecl "__str_less" (Simple(Num)) [t1; t2]
			| Leq -> build_fdecl "__str_lessequal" (Simple(Num)) [t1; t2]
			| Greater -> build_fdecl "__str_greater" (Simple(Num)) [t1; t2]
			| Geq -> build_fdecl "__str_greaterequal" (Simple(Num)) [t1; t2]
			| Eq -> build_fdecl "__str_equal" (Simple(Num)) [t1; t2]
			| Neq -> build_fdecl "__str_notequal" (Simple(Num)) [t1; t2]
			| Plus -> build_fdecl "__str_concat" t1 [t1; t2]
			| Divide ->build_fdecl "__str_match" t1 [t1; t2]
			| Mod -> build_fdecl "__str_index" (Simple(Num)) [t1; t2]
			| _ -> binop_error t1 t2 op) in
			FuncCall(f, [c1; c2])
		| (Simple(Char), Simple(Int)) -> (* string binops with 1 string operand *)
			(match op with
			Minus -> let f = build_fdecl "__str_substr" t1 [t1; t2] in
				FuncCall(f, [c1; c2])
			| _ -> binop_error t1 t2 op)
		| (Map(k1,v1), Map(k2,v2)) -> (* map binops *)
			if k1 = k2 && v1 = v2 then
			let f = (match op with
				Eq -> build_fdecl "__map_equal" (Simple(Num)) [t1; t2]
				| Neq -> build_fdecl "__map_notequal" (Simple(Num)) [t1; t2]
				| _ -> binop_error t1 t2 op) in
			FuncCall(f, [c1; c2])
			else binop_error t1 t2 op
		| (Map(k, v), Simple(l)) ->
			if k = l then
				let f = (match op with
					Minus -> build_fdecl "__map_remove" (Simple(v)) [t1; t2]
					| Mod -> build_fdecl "__map_exists" (Simple(Num)) [t1; t2]
					| _ -> binop_error t1 t2 op) in
				FuncCall(f, [c1; c2])
			else binop_error t1 t2 op
		| _ -> binop_error t1 t2 op


(* Validate the Unary op *)
let check_unop (c:expr) (op:Ast.uop) =
	let t = type_of_expr c in
	match t with
		Simple(Num) ->
			(match op with
				(Neg | Not) -> Unop(Simple(Num), c, op)
				| _ -> unop_error t op)
		| Simple(Str) ->
			(match op with
				Len -> let f = build_fdecl "__str_len" (Simple(Num)) [t] in
					FuncCall(f, [c])
				| _ -> unop_error t op)
		| Map(k, v) ->
			let f = (match op with
				Len -> build_fdecl "__map_len" (Simple(Num)) [t]
				| Keys -> build_fdecl "__map_keys" (Map(Num, k)) [t]
				| Vals -> build_fdecl "__map_vals" (Map(Num, v)) [t]
				| _ -> unop_error t op) in
				FuncCall(f, [c])
		| _ -> unop_error t op
	
=======

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
>>>>>>> 88ff4fd8c132a861403f5a599f8f22edced1f3e9
let rec compare_arglists formals actuals =
	match (formals,actuals) with
	([],[]) -> true
	| (head1::tail1, head2::tail2)
		-> (head1 = head2) && compare_arglists tail1 tail2
	| _ -> false

<<<<<<< HEAD
and check_func (name:string) (cl:expr list) env =
=======
(*checks that a function declaration and calling is proper, such that a function is called with the proper number and type of arguments*)
and check_func (name:string) (cl:c_expr list) env =
>>>>>>> 88ff4fd8c132a861403f5a599f8f22edced1f3e9
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

<<<<<<< HEAD
(* Verify that lvalues are valid *)
and check_lvalue (lv:lvalue) (checked:expr) env =
	let (name,e) = lv in
	let decl = Symtab.symtab_find name env in
	let var = (match decl with VarDecl(v) -> v
			| _ -> raise(Failure("symbol " ^ name ^ " is not a variable"))) in
	let (_,base_t,_) = var in
	let t = type_of_expr checked in
	match t with
		Simple(None) -> (base_t, (var, checked))
		| _ ->
			match base_t with
				Map(k, v) ->
					if t = Simple(k) then (Simple(v), (var, checked))
					else raise(Failure("map type does not match accessor type"))
				| _ -> raise(Failure("cannot apply accessor to non-map"))


(* Check that our expressions (as defined in parser are 
	semantically valid *)
and check_expr (e:expr) env =
	match e with
	Ast.Char_Literal(s) -> Char_Literal(s)
	| Ast.Int_Literal(n) -> Int_Literal(n)
	| Ast.Float_Literal(f) -> Float_Literal(f)
	| Ast.Bool_Literal(b) -> Bool_Literal(b)
	| Ast.NoExpr -> NoExpr
	| Ast.Replace(e1, e2, e3) ->
		let c1, c2 = (check_expr e1 env, check_expr e2 env) in
		let c3 = check_expr e3 env in
		let (t1, t2, t3) = (type_of_expr c1, type_of_expr c2, type_of_expr c3) in
		if(t1 = t2 && t2 = t3 && t3 = Simple(Char)) then
			let f = build_fdecl "__str_replace" t1 [t1; t2; t3] in
			FuncCall(f, [c1; c2; c3])
		else raise(Failure("operator ~ requires 3 string expressions"))
	| Ast.Binop(e1, op, e2) ->
=======
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
>>>>>>> 88ff4fd8c132a861403f5a599f8f22edced1f3e9
		let (c1, c2) = (check_expr e1 env, check_expr e2 env) in
		check_binop c1 c2 op
	| Ast.Unop(e1, op) ->
		let checked = check_expr e1 env in
		check_unop checked op
<<<<<<< HEAD
	| Ast.Rvalue(l) ->
		let checked = check_expr (snd l) env in
		let (result_t, lv) = check_lvalue l checked env in
		Rvalue(result_t, lv)
=======
        | Ast.Tree(t) ->
                check_tree t
>>>>>>> 88ff4fd8c132a861403f5a599f8f22edced1f3e9
	| Ast.Assign(l, ae) ->
		let checked = check_expr ae env in
		let deref = check_expr (snd l) env in
		let (result_t, lv) = check_lvalue l deref env in
		let t = type_of_expr checked in
		if t = result_t then Assign(t, lv, checked)
		else 
			(match (checked, result_t) with
<<<<<<< HEAD
				(Int_Literal(0), Map(_,_)) -> let f = build_fdecl "__map_empty" result_t [result_t] in
				FuncCall(f, [Rvalue(result_t, lv)])
			| _ -> raise(Failure("assignment not compatible with expressions of type " ^
			string_of_type result_t ^ " and " ^ string_of_type t)))
	| Ast.FuncCall(name, el) ->
		let checked = check_exprlist el env in
		check_func name checked env
	
(*Do the same for a list of expressions *)	
=======
				(NumLiteral(0), Map(_,_)) -> let f = build_fdecl "__map_empty" result_t [result_t] in
				FuncCall(f, [Rvalue(result_t, lv)])
			| _ -> raise(Failure("assignment not compatible with expressions of type " ^
			string_of_type result_t ^ " and " ^ string_of_type t)))
	| Ast.Call(name, el) ->
		let checked = check_exprlist el env in
		check_func name checked env
	| Ast.NoExpr -> NoExpr
		
>>>>>>> 88ff4fd8c132a861403f5a599f8f22edced1f3e9
and check_exprlist (el:expr list) env =
	match el with
	[] -> []
	| head :: tail -> (check_expr head env) :: (check_exprlist tail env)

(* check a single statement *)
let rec check_stmt (s:stmt) ret_type env =
	match s with
<<<<<<< HEAD
	Ast.CodeBlock(b) -> CodeBlock(check_block b ret_type env)
	| Ast.Loop(e, b) -> let checked = check_expr e env in
		if type_of_expr checked = Simple(Num) then Loop(checked, check_block b ret_type env)
		else raise(Failure("loop condition expression must be numeric"))
	| Ast.Conditional(e, b1, b2) -> let checked = check_expr e env in
		if type_of_expr checked = Simple(Num) then Conditional(checked, check_block b1 ret_type env, check_block b2 ret_type env)
		else raise(Failure("if condition expression must be numeric"))
	| Ast.Return(e) -> let checked = check_expr e env in
		let t = type_of_expr checked in
		if t = ret_type then Return(checked) else
			raise(Failure("function return type " ^ string_of_type ret_type ^ " not compatible with expression of type " ^ string_of_type t))
	| Ast.Expression(e) -> Expression(check_expr e env)
=======
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
>>>>>>> 88ff4fd8c132a861403f5a599f8f22edced1f3e9

(* check a list of statements *)
and check_stmtlist (s:stmt list) (ret_type:var_type) env =
	match s with
	[] -> []
	| head :: tail -> check_stmt head ret_type env :: check_stmtlist tail ret_type env

<<<<<<< HEAD
=======
(*checks a variable declaration*)
>>>>>>> 88ff4fd8c132a861403f5a599f8f22edced1f3e9
and check_vdecllist (v:var list) env =
	match v with
	[] -> []
	| head :: tail ->
		let decl = Symtab.symtab_find (fst head) env in
		match decl with
			FuncDecl(f) -> raise(Failure("symbol is not a variable"))
			| VarDecl(v) -> v :: check_vdecllist tail env

<<<<<<< HEAD
(* check that function declarations are correct *)
=======
(*checks a function declaration*)
>>>>>>> 88ff4fd8c132a861403f5a599f8f22edced1f3e9
and check_fdecl (f:string) env =
	let decl = Symtab.symtab_find f env in
	match decl with
		VarDecl(v) -> raise(Failure("symbol is not a function"))
		| FuncDecl(f) -> f

(* check a block *)
and check_block (b:block) (ret_type:var_type) env =
	let vars = check_vdecllist b.locals (fst env, b.block_id) in
<<<<<<< HEAD
	{ locals = vars;
		statements = check_stmtlist b.statements ret_type (fst env, b.block_id);
		block_id = b.block_id}
=======
	{ c_locals = vars;
		c_statements = check_stmtlist b.statements ret_type (fst env, b.block_id);
		c_block_id = b.block_id}
>>>>>>> 88ff4fd8c132a861403f5a599f8f22edced1f3e9

(* check a function *)
and check_func (f:func) env =
	let checked = check_block f.body f.ret_type env in
	let formals = check_vdecllist f.formals (fst env, f.body.block_id) in
<<<<<<< HEAD
	{ header = check_fdecl f.name env; body = checked; formals = formals }
=======
	{ c_header = check_fdecl f.name env; c_body = checked; c_formals = formals }
>>>>>>> 88ff4fd8c132a861403f5a599f8f22edced1f3e9

(* check a function list *)
and check_funclist (funcs:func list) env =
	match funcs with
	[] -> []
	| head :: tail -> check_func head env :: check_funclist tail env

<<<<<<< HEAD
(* Vrify that main function is correct *)
and check_main (f:func list) =
	if (List.filter main_fdecl f) = [] then false
	else true

(* Verify that program is correct *)
let check_program (p:program) env =
	let vars = check_vdecllist p.globals env in
	let checked = check_funclist p.functions env in
	if (check_main checked) then {globals = vars; functions = checked; block_count = p.block_count}
	else raise(Failure("function main(^) -> # not found"))
=======
and check_main (f:c_func list) =
	if (List.filter main_fdecl f) = [] then false
	else true

let check_program (p:program) env =
	let vars = check_vdecllist p.globals env in
	let checked = check_funclist p.functions env in
	if (check_main checked) then {c_globals = vars; c_functions = checked; c_block_count = p.block_count}
	else raise (Failure("function main(^) -> # not found"))
>>>>>>> 88ff4fd8c132a861403f5a599f8f22edced1f3e9
