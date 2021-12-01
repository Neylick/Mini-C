%{
  open Lexing
  open Minic_ast
%}

(* Déclaration des lexèmes *)

%token <int> CST
%token <bool> BOOL_CST
%token <string> IDENT

%token WHILE IF ELSE
%token LPAR RPAR BEGIN END
%token RETURN SET SEMI
%token EOF

%token INT BOOL VOID

%token LT GT 
%token LET GET EQ
%token AND OR BAND BOR XOR BXOR NEQ BNEQ

%token ADD MUL DIV MOD SUB

%start program
%type <Minic_ast.prog> program

%%

program:
| dl = declaration_list EOF 
  { let var_list, fun_list = dl in 
    { 
      globals = var_list; 
      functions = fun_list; 
    } 
  }
| error 
  { 
    let pos = $startpos in
    let message = Printf.sprintf "Syntax error at %d, %d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol) in 
    failwith message 
  }
;

declaration_list:
| { [], [] }  (* vide *)
| vd = variable_decl dl = declaration_list { let vl, fl = dl in vd :: vl, fl }
| fd = function_decl dl = declaration_list { let vl, fl = dl in vl, fd :: fl }
;


typ:
  | INT { Int }
  | BOOL { Bool }
  | VOID { Void }
;

(* Decls and sets *)
variable_decl:
  | t = typ i = IDENT SET v = expression SEMI { (t, i, v) }
  | t = typ i = IDENT SEMI { (t, i, Undef) }
  | i = IDENT SET v = expression SEMI { (None, i, v) }
;

(* 
   TODO Params, verif return is here.
*)
function_decl:
  | t = typ f = IDENT LPAR RPAR BEGIN s = list(instruction) END
    { { 
        name = f; 
        code = s; 
        params = []; 
        return = t;
    } }
;
instruction:
(* Return *)
  | RETURN e=expression SEMI { Return(e) }
  | RETURN BEGIN e=expression END SEMI { Return(e) }
  | RETURN SEMI { Return(Undef) }
(* If *)
  | IF LPAR c=expression RPAR BEGIN e1 = list(instruction) END ELSE BEGIN e2 = list(instruction) END { If(c,e1,e2) }
  | IF LPAR c=expression RPAR BEGIN e = list(instruction) END { If(c, e, []) }
  | IF LPAR c=expression RPAR e1 = instruction ELSE e2 = instruction { If(c, [e1], [e2]) } 
  | IF LPAR c=expression RPAR e = instruction  { If(c, [e], []) } 
  | IF LPAR c=expression RPAR SEMI { Skip } 
(* While *)
  | WHILE LPAR c=expression RPAR BEGIN e = list(instruction) END { While(c,e) }
  | WHILE LPAR c=expression RPAR SEMI { While(c, []) }
(* Decls and sets *)
  | decl = variable_decl { Set(decl) }
;

expression:
  | LPAR e=expression RPAR { e }
  | n=CST { Cst(n) }
  | b=BOOL_CST { BCst(b) }
  | i=IDENT { Get(i) }
(* Bool op *)
  | a=expression LT b=expression  {Lt(a,b)} 
  | a=expression GT b=expression  {Gt(a,b)}
  | a=expression LET b=expression  {Leqt(a,b)}
  | a=expression GET b=expression  {Geqt(a,b)}
  | a=expression EQ b=expression  {Eq(a,b)}

  | a=expression AND b=expression  {And(a,b)}
  | a=expression BAND b=expression  {BAnd(a,b)}
  | a=expression OR b=expression  {Or(a,b)}
  | a=expression BOR b=expression  {BOr(a,b)}
  | a=expression XOR b=expression  {Xor(a,b)}
  | a=expression BXOR b=expression  {BXor(a,b)}
  | a=expression NEQ b=expression  {Neq(a,b)}
  | a=expression BNEQ b=expression  {BNeq(a,b)}
(* Int op *)
  | a=expression ADD b=expression  {Add(a,b)}
  | a=expression MUL b=expression  {Mul(a,b)}
  | a=expression DIV b=expression  {Div(a,b)}
  | a=expression MOD b=expression  {Mod(a,b)}
  | a=expression SUB b=expression  {Sub(a,b)}
;
