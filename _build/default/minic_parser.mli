
(* The type of tokens. *)

type token = 
  | XOR
  | WHILE
  | VOID
  | SUB
  | SET
  | SEMI
  | RPAR
  | RETURN
  | OR
  | NEQ
  | MUL
  | MOD
  | LT
  | LPAR
  | LET
  | INT
  | IF
  | IDENT of (string)
  | GT
  | GET
  | EQ
  | EOF
  | END
  | ELSE
  | DIV
  | CST of (int)
  | BXOR
  | BOR
  | BOOL_CST of (bool)
  | BOOL
  | BNEQ
  | BEGIN
  | BAND
  | AND
  | ADD

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Minic_ast.prog)
