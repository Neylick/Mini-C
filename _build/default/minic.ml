let () =
  for arg_idx = 1 to 10 do
    try 
      let file = Sys.argv.(arg_idx) in
      let in_channel = open_in file in  
      Printf.printf "Source %s : Building lexer...\n" file;
      let lexbuf = Lexing.from_channel in_channel in
      
      Printf.printf "Source %s : Creating AST...\n" file;
      let ast = Minic_parser.program Minic_lexer.token lexbuf in
      close_in in_channel;
      Printf.printf "Source %s : Generated AST \n" file;
      Minic_typechecker.typecheck_program ast;
      Printf.printf "Source %s : Type verified\n\n" file;
    with 
    | Invalid_argument _ ->  
      Printf.printf "No more files to work on...\n";
      Printf.printf "exited with code 0\n";
      exit 0
    | err -> raise(err)
  done
