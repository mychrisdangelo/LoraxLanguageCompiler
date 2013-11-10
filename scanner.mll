<<<<<<< HEAD
rule token = parse
     [' ' '\t' '\r' '\n'] { token lexbuf }
| '+' { PLUS }
| '-' { MINUS }
| '*' { TIMES }
| '/' { DIVIDE }
| '=' { ASSIGN }
| '<' { LT }
| '>' { GT }
| "<=" { LTE }
| ">=" { GTE }
| "==" { EQUALS }
| '@' { CHILD }
| '%' { DATA }
| ['0'-'9']+ as lit { INT(int_of_string lit) }
| ['0'-'9']+'.'['0'-'9']+ as lit { DOUBLE(float_of_string lit) }
| 'true'|'false' { TRUE }
| '"'([ _ ]* as lit )'"' { STRING(lit) }
| '''([ _ ]* as lit ) ''' { CHAR(lit) }
=======
{ 
	open Parser 
	exception LexError of string
}

(* Regular Definitions *)

let digit = ['0'-'9']
let decimal = ((digit+ '.' digit*) | ('.' digit+))

(* Regular Rules *)

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf }         (* Whitespace *)
| "/*"       { block_comment lexbuf }           (* Comments *)
| "//"	     { line_comment lexbuf }
| '('        { LPAREN }
| ')'        { RPAREN }
| '{'        { LBRACE }
| '}'        { RBRACE }
| ';'        { SEMI }
| ','        { COMMA }
| '+'        { PLUS }
| '-'        { MINUS }
| '*'        { TIMES }
| "mod"      { MOD } 
| '/'        { DIVIDE }
| '='        { ASSIGN }
| "=="       { EQ }
| "!="       { NEQ }
| '<'        { LT }
| "<="       { LEQ }
| ">"        { GT }
| ">="       { GEQ }
| "if"       { IF }
| "else"     { ELSE }
| "for"      { FOR }
| "while"    { WHILE }
| "return"   { RETURN }
| "int"      { INT }
| "float"    { FLOAT }	
| "string"   { STRING }
| "bool"     { BOOL } 
| "tree"     { TREE } 
| "break"    { BREAK }
| "continue" { CONTINUE }
| "root"     { ROOT }
| "true"	 { TRUE }
| "false"	 { FALSE }
| "null"     { NULL } 
| "char"	 { CHAR }
| "!"		 { NOT } 
| "&&"		 { AND }
| "||"		 { OR }	
| '@'	     { AT }
| '%'		 { CHILD }	
| digit+ as lxm 				{ INT_LITERAL(int_of_string lxm) }
| decimal as lxm 				{ FLOAT_LITERAL(float_of_string lxm) }
| '"'(['_']* as lxm )'"' 		{ STRING_LITERAL(lxm) }
| '''(['_'] as lxm ) ''' 		{ CHAR_LITERAL(lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and block_comment = parse
  "*/" { token lexbuf }
| eof  { raise (LexError("unterminated block_comment!")) }
| _    { block_comment lexbuf }

and line_comment = parse
| ['\n' '\r'] { token lexbuf }
| _                          { line_comment lexbuf }
>>>>>>> master
