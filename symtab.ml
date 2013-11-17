(*
 * Authors:
 * Chris D'Angelo
 * Tim Paine
 * Kira Whithouse
 *)

open Ast

let symtab_of_program (p:Ast.program) =
	"implement me"
	
let string_of_symtab env =
	env

(*
module SymMap = Map.Make(String)
let scope_parents = Array.create 1024 0

let string_of_symtab env =
	let symlist = SymMap.fold
		(fun s t prefix -> (string_of_decl t) :: prefix) (fst env) [] in
	let sorted = List.sort Pervasives.compare symlist in
	String.concat "\n" sorted

let rec symtab_find (name:string) env =
	let(tab, scope) = env in
	let to_find = name ^ "_" ^ (string_of_int scope) in
	if SymMap.mem to_find tab then SymMap.find to_find tab
	else
		if scope = 0 then raise (Failure("symbol " ^ name ^ " not declared in current scope"))
		else symtab_find name (tab, scope_parents.(scope))

let rec symtab_add_decl (name:string) (decl:decl) env =
	let (tab, scope) = env in
	let to_find = name ^ "_" ^ (string_of_int scope) in
	if SymMap.mem to_find tab
		then raise(Failure("symbol " ^ name ^ " declared twice in same scope"))
	else ((SymMap.add to_find decl tab), scope )

(* add list of variables to the symbol table *)	
let rec symtab_add_vars (vars:var list) env =
	match vars with
	[] -> env
	| (name,t) :: tail -> let env = symtab_add_decl name (VarDecl(name, t, snd env)) env in
		symtab_add_vars tail env

(* add declarations inside statements to the symbol table *)
let rec symtab_add_stmts (stmts:stmt list) env =
	match stmts with
	[] -> env
	| head :: tail -> let env = (match head with
		CodeBlock(b) -> symtab_add_block b env
		| Loop(e, b) -> symtab_add_block b env
		| Conditional(e, b1, b2) -> let env = symtab_add_block b1 env in
			symtab_add_block b2 env
		| _ -> env) in symtab_add_stmts tail env

and symtab_add_block (b:block) env =
	if(b.block_id != -1) then
		let (tab, scope) = env in
		let env = symtab_add_vars b.locals (tab, b.block_id) in
		let env = symtab_add_stmts b.statements env in
		scope_parents.(b.block_id) <- scope; ((fst env), scope)
	else env

and symtab_add_func (f:func) env =
	let scope = snd env in
	let args = List.map snd f.formals in
	let env = symtab_add_decl f.name (FuncDecl(f.name, f.ret_type, args, scope)) env in
	let env = symtab_add_vars f.formals ((fst env), f.body.block_id) in
	symtab_add_block f.body ((fst env), scope)

(* add list of functions to the symbol table *)
and symtab_add_funcs (funcs:func list) env =
	match funcs with
	[] -> env
	| head :: tail -> let env = symtab_add_func head env in
		symtab_add_funcs tail env

(* add builtin functions to the symbol table *)
let add_builtins env =
	let env = symtab_add_decl "read" (FuncDecl("read", Simple(Str), [], 0)) env in
	let env = symtab_add_decl "end_input" (FuncDecl("end_input", Simple(Num), [], 0)) env in
	let env = symtab_add_decl "write" (FuncDecl("write", Simple(None), [Simple(Str)], 0)) env in
	let env = symtab_add_decl "to_string" (FuncDecl("to_string", Simple(Str), [Simple(Num)], 0)) env in
	let env = symtab_add_decl "to_num" (FuncDecl("to_num", Simple(Num), [Simple(Str)], 0)) env in
	let env = symtab_add_decl "open" (FuncDecl("open", Simple(None), [Simple(Str); Simple(Str)], 0)) env in
	symtab_add_decl "exit" (FuncDecl("exit", Simple(None), [Simple(Num)], 0)) env

let symtab_of_program (p:Ast.program) =
	let env = add_builtins(SymMap.empty, 0) in
	let env = symtab_add_vars p.globals env in
	symtab_add_funcs p.functions env


*)