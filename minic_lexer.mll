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
    ] ;
    fun s ->
      try  Hashtbl.find h s
      with Not_found -> IDENT(s) 
}

let digit = ['0'-'9']
let hexdigit = ['0'-'9'] | ['a' - 'f'] | ['A' - 'F']
let octdigit = ['0'-'7']

let hexnumber = ("0x" | "0X") hexdigit+   
let octnumber = '0' octdigit*   
let number = (['-']? digit+) | hexnumber | octnumber 

let alpha = ['a'-'z' 'A'-'Z']
let ident = alpha (alpha | '_' | digit)*

rule token = parse
  | ['\n'] { new_line lexbuf; token lexbuf }
  | [' ' '\t' '\r']+ { token lexbuf }
  | number as n { CST(int_of_string n) }
  | ident as id { resolve_keyword id }
  | ";" { SEMI }
  | "=" { SET }
  | "(" { LPAR }
  | ")" { RPAR }
  | "{" { BEGIN }
  | "}" { END }
  | "," { SEPARATOR }
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
