{
  open Lexing
  open Minic_parser

  let resolve_keyword =
    let h = Hashtbl.create 32 in
    List.iter (fun (str, tokenize) -> Hashtbl.add h str tokenize)
    [ 
      (* Obligatoires *)
      "return",   RETURN;
      "true",     BOOL_CST true;
      "false",    BOOL_CST false;
      "int",      INT;
      "bool",     BOOL;
      "if",       IF;
      "else",     ELSE;
      "while",    WHILE;
      "void",     VOID;
      "putchar",  PUTCHAR;

      (* Ajouts *)
      "for",      FOR;
      "do",       DO;
      "switch",   SWITCH;
      "case",     CASE;
      "default",  DEFAULT;
      "break",    BREAK;
      "continue", CONTINUE;
    ] ;
    fun s ->
      try  Hashtbl.find h s
      with Not_found -> IDENT(s) 
}

let digit = ['0'-'9']
let hexdigit = ['0'-'9'] | ['a' - 'f'] | ['A' - 'F']
let octdigit = ['0'-'7']
let bindigit = ['0' '1']

let hexnumber = ("0x" | "0X") hexdigit+   
let octnumber = '0' octdigit*  
let binnumber = ("0b" | "0B") bindigit ('_'? bindigit) (* You can put '_' between binary digits in C *)
let number = (['-']? digit+) | hexnumber |binnumber (* Ocaml's string_to_int already does the conversion from binary and hex *)

let alpha = ['a'-'z' 'A'-'Z']
let ident = alpha (alpha | '_' | digit)*

let line_comment = ("//" [^'\n']* '\n')

rule tokenize = parse
  | ['\n'] { new_line lexbuf; tokenize lexbuf }
  | [' ' '\t' '\r']+ { tokenize lexbuf }
  | line_comment { new_line lexbuf; tokenize lexbuf }
  | "/*" { comment_block lexbuf; tokenize lexbuf }
  | octnumber as n { CST(int_of_string ("0o"^n)) } (* Caml has a different syntax for octals, need to do the conversion *)
  | number as n { CST(int_of_string n) }
  | ident as id { resolve_keyword id }
  | ";" { SEMI }
  | ":" { DOTS2 }
  | "," { SEPARATOR }
  | "=" { SET }
  | "(" { LPAR }
  | ")" { RPAR }
  | "{" { BEGIN }
  | "}" { END }
  (* Ops *)
    | '!' { NOT }
    | '~' { BNOT }
    | '+' { ADD }
    | '*' { MUL }
    | '/'  { DIV }
    | '%'  { MOD }
    | '-' { SUB }

    | '<' { LT }
    | '>' { GT }
    | "<="  { LET }
    | ">=" { GET }
    | "==" { EQ }

    | "!=" { NEQ }
    | '~' { BNEQ }
    | "||" { OR }
    | '|' { BOR }
    | "&&" { AND }
    | '&' { BAND }
    | "^=" { XOR }
    | '^' { BXOR }

  | eof { EOF }
  | _ { failwith ("Unknown character : " ^ (lexeme lexbuf)) }

and comment_block = parse
| "*/" { }
| "/*" { comment_block lexbuf; comment_block lexbuf}
| ['\n'] { new_line lexbuf; comment_block lexbuf }
| _ { comment_block lexbuf }

(* 
  we don't have an eof case because we don't want to prevent 
  compilation if comments are not closed 
*)