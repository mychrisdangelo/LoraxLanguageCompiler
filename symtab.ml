(* 
 * Authors:
 * Tim Paine
 * Chris D'Angelo
 * Special thanks to Dara Hazeghi's strlang and Stephen Edward's MicroC
 * which provided background knowledge.
 *)

open Ast

(* 
 * SymMap contains string : Ast.decl pairs representing
 * identifiername_scopenumber : decl
 *)
module SymMap = Map.Make(String)

let scope_parents = Array.create 1000 0


(* string_of_vdecl from ast.ml *)
let string_of_decl = function
	  SymTab_VarDecl(n, t, id)     -> string_of_vdecl (n, t)
	| SymTab_FuncDecl(n, t, f, id) -> 
	  (string_of_var_type t) ^ " " ^ 
      n ^ "(" ^ 
      String.concat ", " (List.map string_of_var_type f) ^ ")"

let string_of_symtab env =
	let symlist = SymMap.fold
		(fun s t prefix -> (string_of_decl t) :: prefix) (fst env) [] in
	let sorted = List.sort Pervasives.compare symlist in
	String.concat "\n" sorted

let rec symtab_get_id (name:string) env = 
	let(table, scope) = env in
	let to_find = name ^ "_" ^ (string_of_int scope) in
	if SymMap.mem to_find table then scope
	else
		if scope = 0 then raise (Failure("symbol " ^ name ^ " not declared in current scope"))
		else symtab_get_id name (table, scope_parents.(scope))
(*
 * Look for the symbol in the given environment and scope
 * then recursively check in all ancestor scopes 
 *)
let rec symtab_find (name:string) env =
	let(table, scope) = env in
	let to_find = name ^ "_" ^ (string_of_int scope) in
	if SymMap.mem to_find table then SymMap.find to_find table 
	else
		if scope = 0 then raise (Failure("symbol " ^ name ^ " not declared in current scope"))
		else symtab_find name (table, scope_parents.(scope))

let rec symtab_add_decl (name:string) (decl:decl) env =
	let (table, scope) = env in (* get current scope and environment *)
	let to_find = name ^ "_" ^ (string_of_int scope) in
	if SymMap.mem to_find table then raise(Failure("symbol " ^ name ^ " declared twice in same scope"))
	else ((SymMap.add to_find decl table), scope)

(* 
 * recursively add list of variables to the symbol table along with the scope of
 * the block in which they were declared
 *)	
let rec symtab_add_vars (vars:var list) env =
	match vars with
	  [] -> env
	| (vname, vtype) :: tail -> let env = symtab_add_decl vname (SymTab_VarDecl(vname, vtype, snd env)) env in (* name, type, scope *)
		symtab_add_vars tail env 

(* add declarations inside statements to the symbol table *)
let rec symtab_add_stmts (stmts:stmt list) env =
	match stmts with
	  [] -> env (* block contains no statements *)
	| head :: tail -> let env = (match head with
		CodeBlock(s) -> symtab_add_block s env (* statement is an arbitrary block *)
		| For(e1, e2, e3, s) -> symtab_add_block s env (* add the for's block to the record *)
		| While(e, s) -> symtab_add_block s env (* same deal as for *)
		| If(e, s1, s2) -> let env = symtab_add_block s1 env in symtab_add_block s2 env (* add both of if's blocks separately *)
        | _ -> env) in symtab_add_stmts tail env (* return, continue, break, etc *)

and symtab_add_block (b:block) env =
	let (table, scope) = env in 
	let env = symtab_add_vars b.locals (table, b.block_id) in 
	let env = symtab_add_stmts b.statements env in 
    scope_parents.(b.block_id) <- scope; (* parent is block_id - 1 *)
    ((fst env), scope) (* return what we've made *)

and symtab_add_func (f:func) env =
	let scope = snd env in
	let args = List.map snd f.formals in (* gets name of every formal *)
	let env = symtab_add_decl f.fname (SymTab_FuncDecl(f.fname, f.ret_type, args, scope)) env in (* add current function to table *)
	let env = symtab_add_vars f.formals ((fst env), f.fblock.block_id) in (* add vars to the next scope in. scope_id is ahead by one *)
	symtab_add_block f.fblock ((fst env), scope) (* add body to symtable given current environment and scope *) 

(* add list of functions to the symbol table *)
and symtab_add_funcs (funcs:func list) env =
	match funcs with
	   [] -> env
	 | head :: tail -> let env = symtab_add_func head env in 
	   symtab_add_funcs tail env

(* add builtin functions to the symbol table *)
(* these are 'dummy' function declarations with potentially incorrect return/argument types,
   they will be 'filled in' in semantic checking *)
let add_builtins env =
    let env = symtab_add_decl "print" (SymTab_FuncDecl("print", Lrx_Atom(Lrx_Int), [], 0)) env in
    let env = symtab_add_decl "root" (SymTab_FuncDecl("root", Lrx_Atom(Lrx_Int), [], 0)) env in
    symtab_add_decl "degree" (SymTab_FuncDecl("degree", Lrx_Atom(Lrx_Int), [], 0)) env
      
(* 
 * env: Ast.decl Symtab.SymMap.t * int = (<abstr>, 0)
 * the "int" is used to passed from function to function
 * to remember the current scope. it is not used outside this
 * file
 *)
let symtab_of_program (p:Ast.program) =
	let env = add_builtins (SymMap.empty, 0) in
	let env = symtab_add_vars (fst p) env in
	symtab_add_funcs (snd p) env