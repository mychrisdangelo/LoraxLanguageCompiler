(* abstract syntax tree
 	-defines the AST produces by the parser
	-includes functions to dump the AST to screen
*)

type bop = Plus | Minus | Times | Divide | Mod | Less | Greater | Leq | Geq |
	Eq | Neq | Or | And

type uop = Neg | Not | Len | Keys | Vals

let depth = ref 0

type simple_type =
	Str
	| Num
	| None (* expressions with no type - void functions *)

type var_type =
	Map of simple_type * simple_type
	| Simple of simple_type

type var = string * var_type

type var_decl = string * var_type * int
type func_decl = string * var_type * var_type list * int

type decl =
	FuncDecl of func_decl
	| VarDecl of var_decl

type expr =
	StrLiteral of string
	| NumLiteral of int
	| Replace of expr * expr * expr (* string matching *)
	| Binop of expr * bop * expr
	| Unop of expr * uop
	| Assign of lvalue * expr (* lvalue and right side *)
	| FuncCall of string * expr list
	| Rvalue of lvalue (* variable (on the right side) *)
	| NoExpr

and lvalue = string * expr (* expression is optional - map accessor *)

type stmt =
	CodeBlock of block
	| Loop of expr * block
	| Conditional of expr * block * block
	| Return of expr
	| Expression of expr

and block = {
	locals: var list;
	statements: stmt list;
	block_id: int;
}

and func = {
	name : string;
	ret_type: var_type;
	body: block;
	formals : var list
}

type program = {
	globals: var list;
	functions: func list;
	block_count: int
}

let rec tabs_helper = function
	0 -> ""
	| x -> "  " ^ tabs_helper (x-1)
	
let rec tabs = function
	0 -> tabs_helper depth.contents
	| x -> ""

let string_of_binop = function
	Plus -> "+" | Minus -> "-" | Times -> "*" | Divide -> "/" | Mod -> "%"
	| Less -> "<" | Greater -> ">" | Leq -> "<=" | Geq -> ">=" | Eq -> "=="
	| Neq -> "!=" | Or -> "|" | And -> "&"

let string_of_unop = function
	Neg -> "-" | Not -> "!" | Len -> "^" | Keys -> "@%" | Vals -> "@@"

let rec string_of_lval = function
	(name, expr) -> name ^ if expr <> NoExpr then "[" ^ string_of_expr expr ^ "]" else ""

and string_of_expr = function
	StrLiteral(s)	-> s
	| NumLiteral(l)		-> string_of_int l
	| Replace(e1, e2, e3) -> string_of_expr e1 ^ " ~ " ^ string_of_expr e2 ^ " ~ " ^ string_of_expr e3
	| Binop(e1, op, e2) -> string_of_expr e1 ^ " " ^ string_of_binop op ^ " " ^ string_of_expr e2
	| Unop(e, o) -> string_of_unop o ^ string_of_expr e
	| Assign(lv, e) -> string_of_lval lv ^ " <- " ^ string_of_expr e
	| FuncCall(f, v) -> f ^ "(" ^ String.concat "; " (List.map string_of_expr v) ^ ")"
	| Rvalue(lv) -> string_of_lval lv
	| NoExpr -> ""

let rec string_of_stmt = function
	CodeBlock(b) -> string_of_block b
	| Loop(e, b) ->
		"\n" ^ tabs 0 ^ "< " ^ string_of_expr e ^ " >\n" ^ string_of_block b
	| Conditional(e, iftrue, iffalse) ->
		"\n" ^ tabs 0 ^ "[" ^ string_of_expr e ^ "]\n" ^ string_of_block iftrue ^
		if iffalse.block_id != -1 then tabs 0 ^ "![]\n" ^	string_of_block iffalse
		else ""
	| Return(e) -> tabs 0 ^ "-> " ^ string_of_expr e ^ ";\n"
	| Expression(e) -> tabs 0 ^ string_of_expr e ^ ";\n"

and string_of_block (b:block) =
	let s = tabs 0 ^ "{\n" in
	depth := depth.contents + 1;
	let s = s ^ string_of_vars b.locals ^ (String.concat "" (List.map string_of_stmt b.statements)) in
	depth := depth.contents - 1; let s = s ^ "" in s ^ tabs 0 ^ "}\n\n"

and string_of_formals = function
		[]  -> "^"
		| formals -> String.concat "; " (List.map string_of_var formals)

and string_of_func (f:func) =
	let formals = string_of_formals f.formals in
	let ret_type = string_of_type f.ret_type in
	let ret_type = if ret_type = "" then "^" else ret_type in
	tabs 0 ^ f.name ^ "(" ^ formals ^ ")"
	^ " -> " ^ ret_type ^ "\n" ^ string_of_block f.body

and string_of_funclist = function
	[] -> ""
	| flist -> String.concat "\n" (List.map string_of_func flist)

and string_of_var (v:var) =
	(string_of_type (snd v)) ^ " " ^ fst v

and string_of_vars = function
	[] -> ""
	| v -> tabs 0 ^ String.concat (";\n" ^ tabs 0) (List.map string_of_var v) ^ ";\n\n"

and string_of_type = function
	Map(k,v)	-> "%[" ^ (string_of_simple_type k) ^ ";" ^ (string_of_simple_type v) ^ "]"
	| Simple(t) -> string_of_simple_type t

and string_of_simple_type = function
	Str -> "$"
	| Num -> "#"
	| None	-> "^"
	
let string_of_decl = function
	VarDecl(n, t, id) -> (string_of_int id) ^ " " ^ n ^ " " ^ string_of_type t
	| FuncDecl(n, t, f, id) ->
		(string_of_int id) ^ " " ^ n ^ " (" ^ String.concat "; " (List.map string_of_type f) ^
		") " ^ string_of_type t
	
let string_of_program (p:program) =
	depth := 0; string_of_vars p.globals ^ string_of_funclist p.functions
