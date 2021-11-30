let () =
  let file = Sys.argv.(1) in
  let in_channel = open_in file in
  let lexbuf = Lexing.from_channel in_channel in
  
  Printf.printf "Starting work...\n";
  (* starting work *)
  let ast = Minic_parser.program Minic_lexer.token lexbuf in
  close_in in_channel;
  Printf.printf "Program %s : Generated AST \n" file;
  Minic_typechecker.typecheck_program ast;
  Printf.printf "Program %s : Type verified\n" file;
  
  exit 0
