rule token = parse
     [' ' '\t' '\r' '\n'] { token lexbuf }
| '+' { PLUS }
| '-' { MINUS }
| '*' { TIMES }
| '/' { DIVIDE }
| '=' { ASSIGN }
| '<' { LT }
| '>' { GT }
| '<=' { LTE }
| '>=' { GTE }
| '==' { EQUALS }
| '@' { CHILD }
| '%' { DATA }
| ['0'-'9']+ as lit { INT(int_of_string lit) }
| ['0'-'9']+'.'['0'-'9']+ as lit { DOUBLE(float_of_string lit) }
| 'true'|'false' { TRUE }
| '"'([ _ ]* as lit )'"' { STRING(lit) }
| '''([ _ ]* as lit ) ''' { CHAR(lit) }