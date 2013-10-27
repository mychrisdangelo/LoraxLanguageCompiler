{ open Parser }

rule token = parse
[' ' '\t' '\r' '\n'] { token lexbuf }
| "/*"     { block_comment lexbuf }  (* Comments *)
| "//"           { line_comment lexbuf }
| '(' { LPAREN }
| '{' {LBRACE }
| ';' { SEMI }
| ".==" { EQUAL }
| ".!=" { NOTEQUAL }
| "=="    { SAME }
| "!="    { NOTSAME }
| ".+"    { PLUS }
| '+'       { CONCAT }
| ".-"    { MINUS }
| '-'       { REMOVE }
| ".*"    { TIMES }
| "./"    { DIVIDE }
| ".<"    { LESS }
| ".<=" { LESSEQ }
| ".>"    { GREATER }
| ".>=" { GREATEREQ }
| '<'       { SMALLER }
| "<="    { SMALLEREQ }
| '>'       { LARGER }
| ">="    { LARGEREQ }
| '!'       { NOT }
| "&&"    { AND }
| "||"    { OR }
| '('   { RPAREN }
| '{'   { RBRACE }
| ","   { SEQUENCE }
| "="    { ASSIGN }
| '@'       { AT }
| '%'       { CHILD }
| "mod" { MOD }
| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR }
| "while"  { WHILE }
| "return" { RETURN }
| "int"      { INT }
| "float"   { FLOAT }
| "string"   { STRING }
| "char"     { CHAR }
| "bool"     { BOOL }
| "tree"   { TREE }
| "break"    { BREAK }
| "continue" { CONTINUE }
| "null" { NULL }
| "true"|"false" as lit { BOOL(bool_of_string lit) }
| ['0'-'9']+ as lit { INT(int_of_string lit) }
| ['0'-'9']+'.'['0'-'9']+ as lit { FLOAT(float_of_string lit) }
| '"'(['_']* as lit )'"' { STRING(lit) }
| '''(['_']* as lit ) ''' { CHAR(lit) }
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
