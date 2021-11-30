%{
  open Lexing
  open Minic_ast
%}

(* Déclaration des lexèmes *)
(* TODO *)
%token <int> CST
%token <bool> BOOL_CST
%token <string> IDENT
%token WHILE IF ELSE
%token LPAR RPAR BEGIN END
%token RETURN SET SEMI
%token EOF

%token INT BOOL

(* Bool op *)
%token LT GT 
%token LET GET EQ
%token AND OR BAND BOR XOR BXOR NEQ BNEQ

(* Int op *)
%token ADD MUL DIV MOD SUB

%start program
%type <Minic_ast.prog> program

%%

program:
| dl = declaration_list EOF { let var_list, fun_list = dl in { globals = var_list; functions = fun_list; } }
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

(* 
   TODO 
   Non constant var
*)
variable_decl:
| t = typ x = IDENT SET n = CST SEMI { (x, t, n) }
| t = typ x = IDENT SEMI { (x, t, 0) }
;

(*
   TODO
   Void
*)
typ:
| INT { Int }
| BOOL { Bool }
;

(* 
   TODO Locals, params
*)
function_decl:
| t = typ f = IDENT LPAR RPAR BEGIN s = list(instruction) END
   { { 
       name = f; 
       code = s; 
       params = []; 
       return = t; 
       locals = [] 
   } }
;

(* 
   TODO
   Return void
   While;
*)
instruction:
| RETURN e=expression SEMI { Return(e) }
| IF c=expression BEGIN e1 = list(instruction) END ELSE BEGIN e2 = list(instruction) END { If(c,e1,e2) }
| IF c=expression BEGIN e = list(instruction) END { If(c,e,[]) } 
| WHILE c=expression BEGIN e = list(instruction) END { While(c,e) }
;

(* 
   TODO : Operators
*)
expression:
| n=CST { Cst(n) }
| b=BOOL_CST { BCst(b) }
| i=IDENT { Get(i) }
(* Bool op *)
| a=expression LT b=expression  { Lt(a,b) } 
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
