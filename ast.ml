(* 
 * Authors:
 * Chris D'Angelo
 * Special thanks to Dara Hazeghi's strlang and Stephen Edward's MicroC
 * which provided background knowledge.
 *)

type op = 
    Add 
  | Sub 
  | Mult 
  | Div 
  | Mod 
  | Equal 
  | Neq 
  | Less 
  | Leq 
  | Greater 
  | Geq
  | Child
  | And
  | Or

type uop =
    Neg
  | Not
  | At
  | Pop

type expr =
    Int_Literal of int
  | Float_Literal of float
  | String_Literal of string
  | Char_Literal of char
  | Bool_Literal of bool
  | Null_Literal
  | Id of string
  | Binop of expr * op * expr
  | Unop of expr * uop
  | Tree of expr * expr list
  | Assign of expr * expr
  | Call of string * expr list
  | Noexpr

type atom_type =
    Lrx_Int
  | Lrx_Float
  | Lrx_Bool
  | Lrx_Char

type tree_decl = {
    datatype : atom_type;
    degree : expr;
}

type var_type =
    Lrx_Tree of tree_decl
  | Lrx_Atom of atom_type

type var = string * var_type

(* 
 * wrappers for use in symtab 
 * var_decl = <<identifier name>> * 
 *            <<data type>> * 
 *            <<block id to be assigned in symtab>>
 * 
 * func_decl = <<identifier name>> * 
 *             <<return data type>> * 
 *             <<formal arg list>> * 
 *             <<block id to be assigned in symtab>>
 *)
type var_decl = string * var_type * int
type func_decl = string * var_type * var_type list * int

type stmt =
    CodeBlock of block 
  | Expr of expr
  | Return of expr
  | If of expr * block * block 
  | For of expr * expr * expr * block 
  | While of expr * block 
  | Continue
  | Break
  
and block = {
    locals : var list;
    statements: stmt list;
}

type func = {
    fname : string;
    ret_type : var_type; 
    formals : var list;
    fblock: block;
}

type program = var list * func list

type decl = 
    FuncDecl of func_decl
  | VarDecl of var_decl

  (*
let string_of_binop = function
        Add -> "+" 
      | Sub -> "-" 
      | Mult -> "*" 
      | Div -> "/" 
      | Mod -> "mod"
      | Child -> "%"
      | Equal -> "==" 
      | Neq -> "!="
      | Less -> "<" 
      | Leq -> "<=" 
      | Greater -> ">" 
      | Geq -> ">="
      | And -> "&&"
      | Or -> "||"

let rec string_of_expr = function
    Int_Literal(l) -> string_of_int l
  | Float_Literal(l) -> string_of_float l
  | String_Literal(l) -> "\"" ^ l ^ "\""
  | Char_Literal(l) -> "\'" ^ (String.make 1) l ^"\'"
  | Bool_Literal(l) -> string_of_bool l
  | Null_Literal -> "null"
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^
      string_of_binop o ^ " " ^
      string_of_expr e2
  | Unop(e, o) -> 
      (match o with 
          Neg -> "-" ^ string_of_expr e
        | Not -> "!" ^ string_of_expr e
        | At -> string_of_expr e ^ "@"
        | Pop -> string_of_expr e ^ "--")
  | Assign(v, e) -> string_of_expr v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Tree(r, cl) -> string_of_expr r ^ "[" ^ String.concat ", " (List.map string_of_expr cl) ^ "]"
  | Noexpr -> ""

let rec string_of_stmt = function
    Block(blk) -> "{\n" ^ String.concat "" (List.map string_of_stmt blk.body) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block(blk)) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | Break -> "break;"
  | Continue -> "continue;"

let string_of_atom_type = function
    Lrx_Int -> "int"
  | Lrx_Float -> "float"
  | Lrx_Bool -> "bool"
  | Lrx_Char -> "char"

let string_of_vdecl v =
    (match (snd v) with
        Lrx_Atom(t) -> string_of_atom_type t ^ " " ^ fst v
      | Lrx_Tree(t) -> "tree <" ^ string_of_atom_type t.datatype ^ ">" ^ fst v ^ "(" ^ string_of_expr t.degree ^ ")"
    )

let string_of_var_type = function
    Lrx_Atom(t) -> string_of_atom_type t
  | Lrx_Tree(t) -> raise (Failure "tree is invalid function return type")

let string_of_fdecl fdecl =
  (string_of_var_type fdecl.ret_type) ^ " " ^ 
  fdecl.fname ^ "(" ^ String.concat ", " (List.map string_of_vdecl fdecl.formals) ^ ")\n{\n" ^
  String.concat ";\n" (List.map string_of_vdecl fdecl.fblock.locals) ^ (if
      (List.length fdecl.fblock.locals) > 0 then ";\n" else "") ^
  String.concat "" (List.map string_of_stmt fdecl.fblock.body) ^
  "}\n"

let string_of_decl = function
  VarDecl(vname, vtype, id) -> (string_of_int id) ^ " " ^ vname ^ " " ^ string_of_var_type vtype
  | FuncDecl(fname, ftype, formals, id) -> (string_of_int id) ^ " " ^ fname ^ " (" ^ String.concat
      "; " (List.map string_of_var_type formals) ^ ") " ^ string_of_var_type ftype

let string_of_program (vars, funcs) =
  String.concat ";\n" (List.map string_of_vdecl vars) ^ (if (List.length vars) > 0 then ";\n" else "") ^
  String.concat "\n" (List.map string_of_fdecl funcs)


*)

let string_of_program (vars, funcs) =
    "Hello, World"

