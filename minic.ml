(* 
  TODO :
  (-)  Bitwise operators
  (-)  Unary operators (Not, bitwise not, adress ? )

  (?) Separer les checks dans un autre checker : type et syntax check ?
  (?) Ligne et colonne dans les messages d'erreur du type checker

  (+) lists
  (+) pointers
  (+) enums
  (+) unions
  (+) structs
  
    
  (@) Warning : Aucun break pour un switch 
  (@) Warning : Manque de break *lors du fallback sur default*

  (#) Preproc parser : define, if, else, endif, error, ifdef, ifndef, undef, include
  (>) Here comes the call to the typecheck
  ($) Assm parser : Traduire en assembleur
*)

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
      print_string "No more files to work on...\n";
      print_string "exited with code 0\n";
      exit 0
    | Failure(msg) ->
      print_string (msg^"\n");
      exit 1
  done
