(*
 * Authors:
 * Chris D'Angelo
 * Tim Paine
 * Kira Whithouse
 *)

open Ast
module Symbols = Map.Make(String)


type env = {
    function_index : int Symbols.t; (* Index for each function *)
    global_index   : int Symbols.t; (* "Address" for global variables *)
    local_index    : int Symbols.t; (* FP offset for args, locals *)
  }

(*function to increment index*)
let rec enum stride n = function
    [] -> []
  | hd::tl -> (n, hd) :: enum stride (n+stride) tl

let translate (globals, functions) =
     let global_indexes = string_map_pairs Symbols.empty (enum 1 0 globals) in

        let built_in_functions = Symbols.add "print" (-1) Symbols.empty in
           let function_indexes = string_map_pairs built_in_functions
           (enum 1 1 (List.map (fun f -> f.fname) functions)) in
            let translate env fdecl =
               let num_formals = List.length fdecl.formals
                 and num_locals = List.length fdecl.locals
                 and local_offsets = enum 1 1 fdecl.locals
                 and formal_offsets = enum (-1) (-2) fdecl.formals in 
                    let env = { env with local_index = string_map_pairs
                       Symbols.empty (local_offsets @ formal_offsets) } in 

let string_map_pairs map pairs =
  List.fold_left (fun m (i, n) -> StringMap.add n i m) map pairs





(* add list of variables to the symbol table *)	
let rec symtab_add_vars (vars:var list) env =
	match vars with
	[] -> env
	| (name,t) :: tail -> let env = symtab_add_decl name (VarDecl(name, t, snd env)) env in
		symtab_add_vars tail env

(*search symbol table*)
let rec symtab_find (name:string) env =
	let(tab, scope) = env in
	let to_find = name ^ "_" ^ (string_of_int scope) in
	if SymMap.mem to_find tab then SymMap.find to_find tab
	else
		if scope = 0 then raise (Failure("symbol " ^ name ^ " not declared in current scope"))
		else symtab_find name (tab, scope_parents.(scope))


(*add a single function to the symbol table*)
and symtab_add_func (f:func_decl) env =
	let scope = snd env in
	let args = List.map snd f.formals in
	let env = symtab_add_decl f.name f.ret_type f.formals f.locals f.body env in
	let env = symtab_add_vars f.formals ((fst env), f.body.block_id) in
	symtab_add_block f.body ((fst env), scope)

(* add list of functions to the symbol table *)
and symtab_add_funcs (funcs:func_decl list) env =
	match funcs with
	[] -> env
	| head :: tail -> let env = symtab_add_func head env in
		symtab_add_funcs tail env

(*add function declaration to symbol table*)
let rec symtab_add_decl (fname:string) (ret_type:var_type) (formals: var list) (locals: var list) (body: stmt list) env =
	let (tab, scope) = env in
	let to_find = name ^ "_" ^ (string_of_int scope) in
	if SymMap.mem to_find tab
		then raise(Failure("symbol " ^ name ^ " declared twice in same scope"))
	else ((SymMap.add to_find function_declaration -- name, parameters, return_type tab), scope )


let built_in_functions env = 
        Symbols.add "print" (-1) in
        let function_indexes = string_map_pairs built_in_functions
                (enum 1 1 (List.map (fun f -> f.fname) functions)) in


let symtab_add_built_in_functions env =
        let env = symtab_add_decl "print" Lrx_Atom(Lrx_Int) var_type env

        (*let env = symtab_add_decl "open" (FuncDecl("open", Simple(None), [Simple(Str); Simple(Str)], 0)) env in*)

let symtab_of_program (p:Ast.program) =
	let env = built_in_functions(Symbols.empty) in
	let env = symtab_add_vars p.globals env in
        let funcs = snd p in
	symtab_add_funcs funcs env

let rec enum stride n = function
    [] -> []
  | hd::tl -> (n, hd) :: enum stride (n+stride) tl

let string_map_pairs map pairs =
  List.fold_left (fun m (i, n) -> StringMap.add n i m) map pairs

(*
module SymMap = Map.Make(String)
let scope_parents = Array.create 1024 0

let string_of_symtab env =
	let symlist = SymMap.fold
		(fun s t prefix -> (string_of_decl t) :: prefix) (fst env) [] in
	let sorted = List.sort Pervasives.compare symlist in
	String.concat "\n" sorted

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

*)
