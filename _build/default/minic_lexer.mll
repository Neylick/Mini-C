{
  open Lexing
  open Minic_parser

  let resolve_keyword =
    let h = Hashtbl.create 17 in
    List.iter (fun (str, token) -> Hashtbl.add h str token)
    [ 
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
let binnumber = ("0b" | "0B") bindigit ('_'? bindigit) (* You can put _ between digits in C *)
let number = (['-']? digit+) | hexnumber |binnumber

let alpha = ['a'-'z' 'A'-'Z']
let ident = alpha (alpha | '_' | digit)*

let line_comment = ("//" [^'\n']* '\n') 
let block_comment = ("/*" _* "*/")

rule token = parse
  | ['\n'] { new_line lexbuf; token lexbuf }
  | [' ' '\t' '\r']+ { token lexbuf }
  | line_comment { new_line lexbuf; token lexbuf }
  | "/*" { comment_block lexbuf; token lexbuf }
  | octnumber as n { CST(int_of_string ("0o"^n)) } (* ocaml has a different syntax for octals, need to do the conversion *)
  | number as n { CST(int_of_string n) }
  | ident as id { resolve_keyword id }
  | ";" { SEMI }
  | "," { SEPARATOR }
  | "=" { SET }
  | "(" { LPAR }
  | ")" { RPAR }
  | "{" { BEGIN }
  | "}" { END }
  (* Ops *)
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