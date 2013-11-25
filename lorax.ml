type action = Ast | Symtab | SAnalysis | Compile | Binary 

let _ =
  let action = if Array.length Sys.argv > 1 then
    List.assoc Sys.argv.(1) [ ("-a", Ast);
            ("-st", Symtab);
            ("-sa", SAnalysis);
			      ("-c", Compile);
			      ("-b", Binary)]
  else Compile in
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.token lexbuf in
  match action with
      Ast        -> let listing = Ast.string_of_program program
                    in print_string listing
    | Symtab     -> let env = Symtab.symtab_of_program program in
                    print_string (Symtab.string_of_symtab env)
    | SAnalysis  -> let env = Symtab.symtab_of_program program in
                    ignore (Check.check_program program env);
                    print_string "Passed Semantic Analysis.\n"
    | Compile    -> let env = Symtab.symtab_of_program program in
                    let checked = Check.check_program program env in
                    let inter_pgrm = Intermediate.intermediate_rep_program checked in
                    let compiled_program = Output.c_of_inter_pgrm inter_pgrm in
                    print_endline compiled_program
    | Binary     -> raise (Failure "binary not implemented")
 
