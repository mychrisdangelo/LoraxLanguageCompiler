(* 
 * Authors:
 * Chris D'Angelo
 * Special thanks to Dara Hazeghi's strlang and Stephen Edward's MicroC
 * which provided background knowledge.
 *)

open Unix

let c_compiler = "gcc"
let c_warnings = "-w"
let c_debug = "-Wall"
let c_includes = "-I"

type action = Ast | Symtab | SAnalysis | Compile | Binary | Help

let usage (name:string) =
  "usage:\n" ^ name ^ "\n" ^
    "        -a source.lrx              (Print AST of source)\n" ^
    "        -t source.lrx              (Print Symbol Table of source)\n" ^
    "        -s source.lrx              (Run Semantic Analysis over source)\n" ^    
    "        -c source.lrx [target.c]   (Compile to c. Second argument optional)\n" ^
    "        -b source.lrx [target.out] (Compile to executable)\n"

let get_compiler_path (path:string) =
  try
    let i = String.rindex path '/' in
    String.sub path 0 i
  with _ -> "."

let _ =
  let action = 
  if Array.length Sys.argv > 1 then
    (match Sys.argv.(1) with
        "-a" -> if Array.length Sys.argv == 3 then Ast else Help
      | "-t" -> if Array.length Sys.argv == 3 then Symtab else Help
      | "-s" -> if Array.length Sys.argv == 3 then SAnalysis else Help
      | "-c" -> if (Array.length Sys.argv == 3) || (Array.length Sys.argv == 4) then Compile else Help
      | "-b" -> if (Array.length Sys.argv == 3) || (Array.length Sys.argv == 4) then Binary else Help
      | _ -> Help)
  else Help in   

  match action with
      Help -> print_endline (usage Sys.argv.(0)) 
    | (Ast | Symtab | SAnalysis | Compile | Binary ) ->
      let input = open_in Sys.argv.(2) in
      let lexbuf = Lexing.from_channel input in
      let program = Parser.program Scanner.token lexbuf in
      (match action with
          Ast -> let listing = Ast.string_of_program program
                 in print_string listing
        | Symtab -> let env = Symtab.symtab_of_program program in
                    print_string (Symtab.string_of_symtab env)
        | SAnalysis -> let env = Symtab.symtab_of_program program in
                    let checked = Check.check_program program env in
                    ignore checked; print_string "Passed Semantic Analysis.\n"
        | Compile -> let env = Symtab.symtab_of_program program in
                     let checked = Check.check_program program env in
                     let inter_pgrm = Intermediate.intermediate_rep_program checked in
                     let compiled_program = Output.c_of_inter_pgrm inter_pgrm in
                     if Array.length Sys.argv == 3 then print_endline compiled_program
                     else let out = open_out Sys.argv.(3) in output_string out compiled_program; close_out out
        | Binary ->  let env = Symtab.symtab_of_program program in
                     let checked = Check.check_program program env in
                     let inter_pgrm = Intermediate.intermediate_rep_program checked in
                     let compiled_program = Output.c_of_inter_pgrm inter_pgrm in
                     let tmp_c_file = Sys.argv.(2) ^ "_lrxtmp.c" in
                     let exec_file_name = if Array.length Sys.argv == 3 then "a.out" else Sys.argv.(3) in
                     let out = open_out tmp_c_file in
                     output_string out compiled_program; close_out out;
                    execvp c_compiler [|c_compiler; c_warnings; c_debug; c_includes ^ (get_compiler_path Sys.argv.(0)); tmp_c_file; "-o"; exec_file_name|] 
        | Help -> print_endline (usage Sys.argv.(0))) (* impossible case *)
 
