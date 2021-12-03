# 1 "minic_lexer.mll"
 
  open Lexing
  open Minic_parser

  let resolve_keyword =
    let h = Hashtbl.create 32 in
    List.iter (fun (str, token) -> Hashtbl.add h str token)
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

# 36 "minic_lexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base =
   "\000\000\221\255\222\255\002\000\001\000\003\000\229\255\003\000\
    \030\000\031\000\237\255\239\255\240\255\241\255\242\255\243\255\
    \244\255\032\000\246\255\247\255\248\255\080\000\155\000\165\000\
    \175\000\096\000\002\000\255\255\252\255\002\000\253\255\047\000\
    \231\000\194\000\254\000\217\000\250\255\091\000\231\255\233\255\
    \232\255\230\255\228\255\226\255\224\255\225\000\252\255\253\255\
    \004\000\094\000\255\255\254\255";
  Lexing.lex_backtrk =
   "\255\255\255\255\255\255\032\000\030\000\028\000\255\255\034\000\
    \021\000\020\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\010\000\255\255\255\255\255\255\006\000\005\000\019\000\
    \004\000\017\000\001\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\004\000\005\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \003\000\003\000\255\255\255\255";
  Lexing.lex_default =
   "\001\000\000\000\000\000\255\255\255\255\255\255\000\000\255\255\
    \255\255\255\255\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\255\255\000\000\000\000\000\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\000\000\000\000\029\000\000\000\255\255\
    \255\255\255\255\255\255\255\255\000\000\255\255\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\046\000\000\000\000\000\
    \255\255\255\255\000\000\000\000";
  Lexing.lex_trans =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\026\000\027\000\026\000\030\000\026\000\000\000\026\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \026\000\007\000\026\000\000\000\000\000\010\000\004\000\043\000\
    \016\000\015\000\011\000\012\000\018\000\023\000\051\000\025\000\
    \024\000\022\000\022\000\022\000\022\000\022\000\022\000\022\000\
    \022\000\022\000\019\000\020\000\009\000\017\000\008\000\044\000\
    \041\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\040\000\039\000\038\000\003\000\035\000\
    \035\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\014\000\005\000\013\000\006\000\042\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\028\000\036\000\036\000\050\000\000\000\029\000\
    \000\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\000\000\000\000\000\000\000\000\021\000\
    \000\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\022\000\022\000\022\000\022\000\022\000\
    \022\000\022\000\022\000\022\000\022\000\022\000\022\000\022\000\
    \022\000\022\000\022\000\022\000\022\000\022\000\022\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\022\000\
    \022\000\000\000\000\000\047\000\000\000\000\000\000\000\000\000\
    \000\000\031\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\022\000\022\000\000\000\000\000\000\000\000\000\
    \002\000\000\000\255\255\000\000\000\000\000\000\000\000\032\000\
    \000\000\036\000\036\000\049\000\000\000\000\000\000\000\000\000\
    \048\000\031\000\000\000\000\000\000\000\000\000\000\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\000\000\000\000\000\000\000\000\000\000\000\000\032\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \037\000\000\000\000\000\000\000\000\000\000\000\000\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\000\000\000\000\000\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\255\255\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000";
  Lexing.lex_check =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\026\000\029\000\000\000\255\255\026\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\000\000\026\000\255\255\255\255\000\000\000\000\004\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\048\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\003\000\
    \007\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\008\000\009\000\017\000\000\000\031\000\
    \031\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\005\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\025\000\037\000\037\000\049\000\255\255\025\000\
    \255\255\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\255\255\255\255\255\255\255\255\021\000\
    \255\255\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
    \021\000\021\000\021\000\022\000\022\000\022\000\022\000\022\000\
    \022\000\022\000\022\000\022\000\022\000\023\000\023\000\023\000\
    \023\000\023\000\023\000\023\000\023\000\023\000\023\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\255\255\255\255\045\000\255\255\255\255\255\255\255\255\
    \255\255\024\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\255\255\255\255\255\255\255\255\
    \000\000\255\255\029\000\255\255\255\255\255\255\255\255\024\000\
    \255\255\035\000\035\000\045\000\255\255\255\255\255\255\255\255\
    \045\000\024\000\255\255\255\255\255\255\255\255\255\255\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\255\255\255\255\255\255\255\255\255\255\255\255\024\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \035\000\255\255\255\255\255\255\255\255\255\255\255\255\034\000\
    \034\000\034\000\034\000\034\000\034\000\255\255\255\255\255\255\
    \032\000\032\000\032\000\032\000\032\000\032\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\034\000\
    \034\000\034\000\034\000\034\000\034\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\045\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255";
  Lexing.lex_base_code =
   "";
  Lexing.lex_backtrk_code =
   "";
  Lexing.lex_default_code =
   "";
  Lexing.lex_trans_code =
   "";
  Lexing.lex_check_code =
   "";
  Lexing.lex_code =
   "";
}

let rec token lexbuf =
   __ocaml_lex_token_rec lexbuf 0
and __ocaml_lex_token_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 52 "minic_lexer.mll"
           ( new_line lexbuf; token lexbuf )
# 213 "minic_lexer.ml"

  | 1 ->
# 53 "minic_lexer.mll"
                     ( token lexbuf )
# 218 "minic_lexer.ml"

  | 2 ->
# 54 "minic_lexer.mll"
                 ( new_line lexbuf; token lexbuf )
# 223 "minic_lexer.ml"

  | 3 ->
# 55 "minic_lexer.mll"
         ( comment_block lexbuf; token lexbuf )
# 228 "minic_lexer.ml"

  | 4 ->
let
# 56 "minic_lexer.mll"
                 n
# 234 "minic_lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 56 "minic_lexer.mll"
                   ( CST(int_of_string ("0o"^n)) )
# 238 "minic_lexer.ml"

  | 5 ->
let
# 57 "minic_lexer.mll"
              n
# 244 "minic_lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 57 "minic_lexer.mll"
                ( CST(int_of_string n) )
# 248 "minic_lexer.ml"

  | 6 ->
let
# 58 "minic_lexer.mll"
             id
# 254 "minic_lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 58 "minic_lexer.mll"
                ( resolve_keyword id )
# 258 "minic_lexer.ml"

  | 7 ->
# 59 "minic_lexer.mll"
        ( SEMI )
# 263 "minic_lexer.ml"

  | 8 ->
# 60 "minic_lexer.mll"
        ( DOTS2 )
# 268 "minic_lexer.ml"

  | 9 ->
# 61 "minic_lexer.mll"
        ( SEPARATOR )
# 273 "minic_lexer.ml"

  | 10 ->
# 62 "minic_lexer.mll"
        ( SET )
# 278 "minic_lexer.ml"

  | 11 ->
# 63 "minic_lexer.mll"
        ( LPAR )
# 283 "minic_lexer.ml"

  | 12 ->
# 64 "minic_lexer.mll"
        ( RPAR )
# 288 "minic_lexer.ml"

  | 13 ->
# 65 "minic_lexer.mll"
        ( BEGIN )
# 293 "minic_lexer.ml"

  | 14 ->
# 66 "minic_lexer.mll"
        ( END )
# 298 "minic_lexer.ml"

  | 15 ->
# 68 "minic_lexer.mll"
          ( ADD )
# 303 "minic_lexer.ml"

  | 16 ->
# 69 "minic_lexer.mll"
          ( MUL )
# 308 "minic_lexer.ml"

  | 17 ->
# 70 "minic_lexer.mll"
           ( DIV )
# 313 "minic_lexer.ml"

  | 18 ->
# 71 "minic_lexer.mll"
           ( MOD )
# 318 "minic_lexer.ml"

  | 19 ->
# 72 "minic_lexer.mll"
          ( SUB )
# 323 "minic_lexer.ml"

  | 20 ->
# 74 "minic_lexer.mll"
          ( LT )
# 328 "minic_lexer.ml"

  | 21 ->
# 75 "minic_lexer.mll"
          ( GT )
# 333 "minic_lexer.ml"

  | 22 ->
# 76 "minic_lexer.mll"
            ( LET )
# 338 "minic_lexer.ml"

  | 23 ->
# 77 "minic_lexer.mll"
           ( GET )
# 343 "minic_lexer.ml"

  | 24 ->
# 78 "minic_lexer.mll"
           ( EQ )
# 348 "minic_lexer.ml"

  | 25 ->
# 80 "minic_lexer.mll"
           ( NEQ )
# 353 "minic_lexer.ml"

  | 26 ->
# 81 "minic_lexer.mll"
          ( BNEQ )
# 358 "minic_lexer.ml"

  | 27 ->
# 82 "minic_lexer.mll"
           ( OR )
# 363 "minic_lexer.ml"

  | 28 ->
# 83 "minic_lexer.mll"
          ( BOR )
# 368 "minic_lexer.ml"

  | 29 ->
# 84 "minic_lexer.mll"
           ( AND )
# 373 "minic_lexer.ml"

  | 30 ->
# 85 "minic_lexer.mll"
          ( BAND )
# 378 "minic_lexer.ml"

  | 31 ->
# 86 "minic_lexer.mll"
           ( XOR )
# 383 "minic_lexer.ml"

  | 32 ->
# 87 "minic_lexer.mll"
          ( BXOR )
# 388 "minic_lexer.ml"

  | 33 ->
# 89 "minic_lexer.mll"
        ( EOF )
# 393 "minic_lexer.ml"

  | 34 ->
# 90 "minic_lexer.mll"
      ( failwith ("Unknown character : " ^ (lexeme lexbuf)) )
# 398 "minic_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_token_rec lexbuf __ocaml_lex_state

and comment_block lexbuf =
   __ocaml_lex_comment_block_rec lexbuf 45
and __ocaml_lex_comment_block_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 93 "minic_lexer.mll"
       ( )
# 410 "minic_lexer.ml"

  | 1 ->
# 94 "minic_lexer.mll"
       ( comment_block lexbuf; comment_block lexbuf)
# 415 "minic_lexer.ml"

  | 2 ->
# 95 "minic_lexer.mll"
         ( new_line lexbuf; comment_block lexbuf )
# 420 "minic_lexer.ml"

  | 3 ->
# 96 "minic_lexer.mll"
    ( comment_block lexbuf )
# 425 "minic_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_comment_block_rec lexbuf __ocaml_lex_state

;;

