/*
 * Authors:
 * Doug Beinstock 
 * Chris D'Angelo
 */

%{ open Ast %}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA
%token PLUS MINUS TIMES DIVIDE MOD ASSIGN POP
%token AND OR NOT
%token EQ NEQ LT LEQ GT GEQ
%token LBRACKET RBRACKET
%token CHAR BOOL INT FLOAT STRING TREE
%token BREAK CONTINUE AT CHILD
%token TRUE FALSE NULL
%token RETURN IF ELSE FOR WHILE
%token <int> INT_LITERAL
%token <bool> BOOL_LITERAL
%token <float> FLOAT_LITERAL
%token <string> STRING_LITERAL
%token <char> CHAR_LITERAL
%token <string> ID
%token EOF

%nonassoc NOELSE
%nonassoc ELSE 
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS 
%left TIMES DIVIDE MOD
%left NEG NOT
%left AT CHILD POP

%start program
%type <Ast.program> program

%%

program:
   /* nothing */ { [], [] }
 | program global_vdecl { ($2 :: fst $1), snd $1 }
 | program fdecl { fst $1, ($2 :: snd $1) }

fdecl:
   var_type ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
     { { fname = $2;
   ret_type = $1;
	 formals = $4;
	 fblock = {locals = List.rev $7;
     body = List.rev $8 } } }

(*block:
	LBRACE var_list stmt_list RBRACE
		{ {locals = List.rev $2; statements = $3; block_id = gen_block_id ()} }
*)
formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    vdecl                   { [$1] }
  | formal_list COMMA vdecl { $3 :: $1 }

vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl SEMI { $2 :: $1 }

global_vdecl:
  vdecl SEMI { $1 }

vdecl:
    var_type ID { ($2, $1) }
  | TREE LT INT GT ID LPAREN expr RPAREN { ($5, Lrx_Tree({datatype = Lrx_Int; degree = $7})) }
  | TREE LT CHAR GT ID LPAREN expr RPAREN { ($5, Lrx_Tree({datatype = Lrx_Char; degree = $7})) }
  | TREE LT BOOL GT ID LPAREN expr RPAREN { ($5, Lrx_Tree({datatype = Lrx_Bool; degree = $7})) }
  | TREE LT FLOAT GT ID LPAREN expr RPAREN { ($5, Lrx_Tree({datatype = Lrx_Float; degree = $7})) }
  | STRING ID { ($2, Lrx_Tree({datatype = Lrx_Char; degree = Int_Literal(1)})) }

var_type:
    INT    { Lrx_Atom(Lrx_Int) }
  | CHAR   { Lrx_Atom(Lrx_Char) }
  | BOOL   { Lrx_Atom(Lrx_Bool) }
  | FLOAT  { Lrx_Atom(Lrx_Float) }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI { Expr($1) }
  | RETURN expr SEMI { Return($2) }
  | LBRACE stmt_list RBRACE { Block(List.rev $2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | FOR LPAREN expr_opt SEMI expr_opt SEMI expr_opt RPAREN stmt
     { For($3, $5, $7, $9) }
  | WHILE LPAREN expr RPAREN stmt { While($3, $5) }
  | BREAK SEMI { Break }
  | CONTINUE SEMI { Continue }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
    literal                      { $1 }
  | tree                         { $1 }
  | ID                           { Id($1) }
  | expr PLUS   expr             { Binop($1, Add, $3) }
  | expr MINUS  expr             { Binop($1, Sub, $3) }
  | expr TIMES  expr             { Binop($1, Mult, $3) }
  | expr DIVIDE expr             { Binop($1, Div, $3) }
  | expr MOD    expr             { Binop($1, Mod, $3) }
  | expr EQ     expr             { Binop($1, Equal, $3) }
  | expr NEQ    expr             { Binop($1, Neq, $3) }
  | expr LT     expr             { Binop($1, Less, $3) }
  | expr LEQ    expr             { Binop($1, Leq, $3) }
  | expr GT     expr             { Binop($1, Greater, $3) }
  | expr GEQ    expr             { Binop($1, Geq, $3) }
  | expr AND    expr             { Binop($1, And, $3) }
  | expr OR     expr             { Binop($1, Or, $3) }
  | MINUS expr %prec NEG         { Unop($2, Neg) }
  | NOT expr                     { Unop($2, Not) }
  | expr CHILD expr              { Binop($1, Child, $3) }
  | expr POP                     { Unop($1, Pop) }
  | expr AT                      { Unop($1, At) }
  | expr ASSIGN expr             { Assign($1, $3) }
  | ID LPAREN actuals_opt RPAREN { Call($1, $3) }
  | LPAREN expr RPAREN { $2 }

literal:
    INT_LITERAL    { Int_Literal($1) }
  | FLOAT_LITERAL  { Float_Literal($1) }
  | STRING_LITERAL { String_Literal($1) }
  | CHAR_LITERAL   { Char_Literal($1) }
  | BOOL_LITERAL   { Bool_Literal($1) }
  | NULL           { Null_Literal }

node_expr:
	  literal            { $1 }
  | ID                 { Id($1) }
	| LPAREN expr RPAREN { $2 }

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }

tree:
    node_expr LBRACKET nodes RBRACKET { Tree($1, $3) }

nodes:
    /* nothing */    { [] }
  | expr             { [$1] }
  | expr COMMA nodes { $1 :: $3 } /* nodes are kept in order */ 
