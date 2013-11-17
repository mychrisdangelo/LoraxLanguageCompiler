type action = Ast | Compile | Binary

let _ =
  let action = if Array.length Sys.argv > 1 then
    List.assoc Sys.argv.(1) [ ("-a", Ast);
			      ("-c", Compile);
			      ("-b", Binary) ]
  else Compile in
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.token lexbuf in
  match action with
      Ast        -> let listing = Ast.string_of_program program
                    in print_string listing
    | Compile    -> let env = Symtab.symtab_of_program program in
                    (print_string (Symtab.string_of_symtab env))
    | Binary     -> raise (Failure "binary not implemented")
 
