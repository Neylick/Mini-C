
(* The type of tokens. *)

type token = 
  | XOR
  | WHILE
  | VOID
  | SWITCH
  | SUB
  | SET
  | SEPARATOR
  | SEMI
  | RPAR
  | RETURN
  | PUTCHAR
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
  | FOR
  | EQ
  | EOF
  | END
  | ELSE
  | DOTS2
  | DO
  | DIV
  | DEFAULT
  | CST of (int)
  | CONTINUE
  | CASE
  | BXOR
  | BREAK
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
