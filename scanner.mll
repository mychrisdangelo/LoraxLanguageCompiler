rule token = parse
[' ' '\t' '\r' '\n'] { token lexbuf }
| "/*"     { block_comment lexbuf }  (* Comments *)
| "//"           { line_comment lexbuf }
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
| "mod" { MOD }
| '!'       { NOT }
| "&&"    { AND }
| "||"    { OR }
| ","   { SEQUENCE }
| "="    { ASSIGN }
| '@'       { AT }
| '%'       { CHILD }
| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR }
| "while"  { WHILE }
| "return" { RETURN }
| "int"      { INT }
| "double"   { DOUBLE }
| "string"   { STRING }
| "tree"   { TREE }
| "tuple"   { TUPLE  }
| "break"    { BREAK }
| "continue" { CONTINUE }
| "root"     { ROOT }
| "children" { CHILDREN }
| "print"         { PRINT }
| "true"         { TRUE }
| "false"         { FALSE }
(*| (('0'-'9') '[' ('0'-'9')* ']')  {TREE}
| ('<' ('0'-'9')+ (',' ('0'-'9')+)*  '>')  {TUPLE}
*)| ('[' '0'-'9' ']') as child {int_of_string child.[1] }
| ('(' '0'-'9' ')') as element {int_of_string element.[1] }
| ['0'-'9']+ as lit { LITERAL(int_of_string lit) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and block_comment = parse
"*/" { token lexbuf }
| eof  { raise (LexError("unterminated block_comment!")) }
| _    { block_comment lexbuf }

and line_comment = parse
| ['\n' '\r'] { token lexbuf }
| _                          { line_comment lexbuf }
