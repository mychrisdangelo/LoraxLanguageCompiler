rule token = parse
[' ' '\t' '\r' '\n'] { token lexbuf }
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
| (('0'-'9') '[' ('0'-'9')* ']')  {TREE}
| ('<' ('0'-'9')+ (',' ('0'-'9')+)*  '>')  {TUPLE}
| ('[' '0'-'9' ']') as child {int_of_string child.[1] }
| ('(' '0'-'9' ')') as element {int_of_string element.[1] }
| ['0'-'9']+ as lit { LITERAL(int_of_string lit) }
| eof { EOF }
