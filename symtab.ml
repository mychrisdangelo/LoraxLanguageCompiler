(* 
 * Authors:
 * Tim Paine
 * Chris D'Angelo
 * Special thanks to Dara Hazeghi's strlang and Stephen Edward's MicroC
 * which provided background knowledge.
 *)

open Ast

(* 
 * Maps a current environment to the scope of the block
 * in which it is contained
 *)
module SymMap = Map.Make(String)

(* activation record data *)
let scope_parents = Array.create 1000 0

(*
 * As the blocks are parsed, they are assigned a unique id to differentiate their
 * scopr from that of other blocks. Symtab uses these block ids as an indicator
 * of scope, and to build activation records recursively
 *)
type scope_block = {
        locals: var list;
        statements: stmt list;
        block_id: int;
}

let scope_id = ref 1
let gen_block_id (u:unit) =
    let x = scope_id.contents in
    scope_id := x + 1; x

(*
(*Print the symbol table of the given environment*)
let string_of_symtab env =
	let symlist = SymMap.fold
		(fun s t prefix -> (string_of_decl t) :: prefix) (fst env) [] in
	let sorted = List.sort Pervasives.compare symlist in
	String.concat "\n" sorted
*)

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
	let (table, scope) = env in (*get current scope and environment*)
	let to_find = name ^ "_" ^ (string_of_int scope) in
	if SymMap.mem to_find table (*if there is a duplicate*)
		then raise(Failure("symbol " ^ name ^ " declared twice in same scope"))
	else ((SymMap.add to_find decl table), scope ) (*else add*)

(* 
 * recursively add list of variables to the symbol table along with the scope of
 * the block in which they were declared
 *)	
let rec symtab_add_vars (vars:var list) env =
	match vars with
	[] -> env
	| (vname,vtype) :: tail -> let env = symtab_add_decl vname (SymTab_VarDecl(vname,vtype, snd env)) env in (*name, type, scope*)
		symtab_add_vars tail env 

(* add declarations inside statements to the symbol table *)
let rec symtab_add_stmts (stmts:stmt list) env =
	match stmts with
	[] -> env (* block contains no statements *)
	| head :: tail -> let env = (match head with
		CodeBlock(s) -> symtab_add_block s env (* statement is an arbitrary block *)
		| For(e1,e2,e3,s) -> symtab_add_block s env (* add the for's block to the record *)
		| While(e, s) -> symtab_add_block s env (* same deal as for *)
		| If(e, s1, s2) -> let env = symtab_add_block s1 env in symtab_add_block s2 env (* add both of if's blocks separately *)
        | _ -> env) in symtab_add_stmts tail env (* return, continue, break, etc *)

and symtab_add_block (b:block) env =
	let (table, scope) = env in (* get current environment *)
    let block_id = gen_block_id() in  
	let env = symtab_add_vars b.locals (table, block_id) in (* add the block's local variables to the table with scope equal to the current block's id *)
	let env = symtab_add_stmts b.statements env in (* add all statements, need to do all subblocks before we do the outer block *)
    scope_parents.(block_id) <- scope; ((fst env), scope)     (* add the current block to the parent scope table i.e. the parent scope of this block is equal to the current scope of the environment *)

and symtab_add_func (f:func) env =
	let scope = snd env in (* current scope is 2nd element in env *)
	let args = List.map snd f.formals in (* gets name of every formal *)
	let env = symtab_add_decl f.fname (SymTab_FuncDecl(f.fname, f.ret_type, args, scope)) env in (* add current function to table *)
	let env = symtab_add_vars f.formals ((fst env), !scope_id) in (* adds vars to table *)
	symtab_add_block f.fblock ((fst env), scope) (* add body to symtable given current environment and scope *) 

(* add list of functions to the symbol table *)
and symtab_add_funcs (funcs:func list) env =
	match funcs with
	[] -> env
	| head :: tail -> let env = symtab_add_func head env in
		symtab_add_funcs tail env

(* add builtin functions to the symbol table *)
let add_builtins env =
    symtab_add_decl "print" (SymTab_FuncDecl("print", Lrx_Atom(Lrx_Int), [Lrx_Tree({datatype = Lrx_Char; degree = Int_Literal(1)})],0)) env 

let symtab_of_program (p:Ast.program) =
	let env = add_builtins (SymMap.empty, 0) in
	let env = symtab_add_vars (fst p) env in
	symtab_add_funcs (snd p) env