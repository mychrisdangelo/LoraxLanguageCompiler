type c_expr =
        Noexpr
        | Null_Literal
        | Int_Literal of int
        | Float_Literal of float
        | String_Literal of string
        | Char_Literal of char
        | Bool_Literal of bool
        | Id of var_type * string
        | Binop of var_type * c_expr * bop * c_expr
        | Unop of var_type * c_expr * uop
        | Tree of var_type * c_expr * c_expr * list
        | Assign of var_type * c_lvalue * expr (* check with chris *)
        | Call of func_decl * c_expr list


type c_stmt =
    Block of c_stmt list
  | Expr of c_expr
  | Return of c_expr
  | If of c_expr * c_stmt * c_stmt
  | For of c_expr * c_expr * c_expr * c_stmt
  | While of c_expr * c_stmt
  | Continue
  | Break

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

        
type c_tree_decl = {
       datatype: atom_type;
       degree: c_expr;
} 

let check_unop (e:c_expr) (op:Ast.uop) = 
        let t = type_of_expr e in
        match t with
                | Lrx_Atom(Lrx_Int) ->
                        (match op with
                                (Neg | Not) -> Unop((Lrx_Atom(Lrx_Int)), e, op)
                                | _ -> unop_error t op)
                | Lrx_Atom(Lrx_Float) ->
                         (match op with
                                (Neg | Not) -> Unop((Lrx_Atom(Lrx_Float)), e, op)
                                | _ -> unop_error t op)
                | Lrx_Tree (a:atom_type) (d:c_expr) ->
                        let t_d = type_of_expr d in
                        match t_d  with
                                |Lrx_Atom(Lrx_Int) ->
                                         (match op with
                                                (Not -> Unop(Lrx_Tree, e, op)) 
                                                | Pop -> build_function "__tree_pop" () [a;d]
                                                | At -> build_function "__tree_at" () [a;d]
                                                | _ -> unop_error t op)
                                | _ -> unop_error t op (*declaring a tree with degree where degree is not type int *)
                                 
               | _ -> unop_error t op
               
let type_of_expr = function
        Noexpr
        Null_Literal
       | Int_Literal(i) -> Lrx_Atom(Lrx_Int) 
       | Float_Literal(i) -> Lrx_Atom(Lrx_Float) 
       | Char_Literal(i) -> Lrx_Atom(Lrx_Char) 
       | Bool_Literal(i) -> Lrx_Atom(Lrx_Bool)
       | String_Literal() -> 
       | Binop(
       | Unop(
       | Id
       | Tree
       | Assign
       | Call


let check_binop (c1:expr) (c2:expr) (op:Ast.bop) =
        let (t1, t2) = (type_of_expr c1, type_of_expr c2) in
    let check_binop (c1:expr) (c2:expr) (op:Ast.bop) =
                (Lrx_Atom(Lrx_Int), Lrx_Atom(Lrx_Int)) -> Binop(Simple(Int), c1, op, c2) (* standard arithmetic binops *)
                | (Lrx_Atom(Lrx_Float), Lrx_Atom(Lrx_Float)) -> (* string binops with 2 string operands *)
                let f = (match op with
                        Add -> build_fdecl "__tree_less" (Return_type) [t1; t2]
                        | Child 
                        | Equal
                        | Neq
                        | Less
                        | Leq
                        | Greater
                        | Geq
                        | _ -> binop_error t1 t2 op) in
                        FuncCall(f, [c1; c2])
                | (Lrx_Atom(Lrx_Bool), Lrx_Atom(Lrx_Bool)) -> (* string binops with 2 string operands *)
                | (Lrx_Atom(Lrx_Char), Lrx_Atom(Lrx_Char)) -> (* string binops with 2 string operands *)

                | ((Lrx_Tree()), Lrx_Atom(Lrx_Int)) (*binop with one tree, one int --> child operator*)
                        
                        
(* CAN TREES EVALUATE TO BOOLEANS -> && and || usage?*) yes -- check if null 
(* RETURN TYPES *) Lrx_int, float, bool, char(LRx types) Lrx_tree/Lrx_Atom
(* WHAT IS SIMPLE*) 


(*can we compare TREE (null) to int/float/string*)
build_fdecl takes name_string return_variable_type argument_variable_type_list
--------------------------------


                        Less -> build_fdecl "__str_less" (Simple(Num)) [t1; t2]
                        | Leq -> build_fdecl "__str_lessequal" (Simple(Num)) [t1; t2]
                        | Greater -> build_fdecl "__str_greater" (Simple(Num)) [t1; t2]
                        | Geq -> build_fdecl "__str_greaterequal" (Simple(Num)) [t1; t2]
                        | Eq -> build_fdecl "__str_equal" (Simple(Num)) [t1; t2]
                        | Neq -> build_fdecl "__str_notequal" (Simple(Num)) [t1; t2]
                        | -> build_fdecl "__str_concat" t1 [t1; t2]
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
