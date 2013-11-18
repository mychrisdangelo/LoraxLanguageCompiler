type action = Ast | Compile | Binary

let _ =
  let action = if Array.length Sys.argv > 1 then
    List.assoc Sys.argv.(1) [ ("-a", Ast);
<<<<<<< HEAD
			      ("-c", Compile) ]
=======
			      ("-c", Compile);
			      ("-b", Binary) ]
>>>>>>> 88ff4fd8c132a861403f5a599f8f22edced1f3e9
  else Compile in
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.token lexbuf in
  match action with
<<<<<<< HEAD
    Ast -> let listing = Ast.string_of_program program
           in print_string listing
(*   | Interpret -> ignore (Interpret.run program)
  | Bytecode -> let listing =
      Bytecode.string_of_prog (Compile.translate program)
    in print_endline listing *)
  | Compile -> Execute.execute_prog (Compile.translate program) 
=======
      Ast        -> let listing = Ast.string_of_program program
                    in print_string listing
    | Compile    -> let env = Symtab.symtab_of_program program in
                    (print_string (Symtab.string_of_symtab env))
    | Binary     -> raise (Failure "binary not implemented")
>>>>>>> 88ff4fd8c132a861403f5a599f8f22edced1f3e9
 
