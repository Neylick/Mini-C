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
%token RETURN SET SEMI SEPARATOR
%token EOF

%token PUTCHAR

%token INT BOOL VOID

%token LT GT 
%token LET GET EQ
%token AND OR BAND BOR XOR BXOR NEQ BNEQ

%token ADD MUL DIV MOD SUB

%start program
%type <Minic_ast.prog> program

%%

program:
| p = global_scope_def_list EOF {p}
| error 
  { 
      let pos = $startpos in
      let message = Printf.sprintf "Syntax error at line %d:%d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol) in 
      failwith message 
  }
;

global_scope_def_list:
| { [] }
| d = global_scope_def l=global_scope_def_list { d::l }
;

global_scope_def:
| vd = variable_decl {Variable(vd)}
| fd = function_decl {Function(fd)}
;

(*

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
    let message = Printf.sprintf "Syntax error at line %d:%d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol) in 
    failwith message 
  }
;

declaration_list:
  | { [], [] }  (* vide *)
  | vd = variable_decl dl = declaration_list { let vl, fl = dl in vd :: vl, fl }
  | fd = function_decl dl = declaration_list { let vl, fl = dl in vl, fd :: fl }
;

*)

typ:
  | INT { Int }
  | BOOL { Bool }
  | VOID { Void }
;

simple_var_decl:
  | t = typ i = IDENT { (t, i, Undef) }

parameter_list:
  | { [] }
  | d=simple_var_decl { let (t, i, _) = d in [(t,i)] } (* No separator in that case *)
  | d=simple_var_decl SEPARATOR p=parameter_list { let (t, i, _) = d in (t,i)::p }

(* Decls and sets *)
variable_decl:
  | t = typ i = IDENT SET v = expression SEMI { (t, i, v) }
  | d = simple_var_decl SEMI { d }
  | i = IDENT SET v = expression SEMI { (None, i, v) }
;

(* Function decls *)
function_decl:
  | t = typ f = IDENT LPAR p = parameter_list RPAR BEGIN s = list(instruction) END
    { { 
        name = f; 
        code = s; 
        params = p; 
        return = t;
    } }
;
instruction:
  (* Return *)
    (* return _; *)
    | RETURN e=expression SEMI { Return(e) }
    (* return(_);*)
    | RETURN BEGIN e=expression END SEMI { Return(e) }
    (* return;*)
    | RETURN SEMI { Return(Undef) }
  (* If *)
    (* if(c) {s1} else {s2}*)
    | IF LPAR c=expression RPAR BEGIN s1 = list(instruction) END ELSE BEGIN s2 = list(instruction) END { If(c,s1,s2) }
    (* if(c) {s} *)    
    | IF LPAR c=expression RPAR BEGIN s = list(instruction) END { If(c, s, []) }
    (* if(c) i1 else i2*)
    | IF LPAR c=expression RPAR i1 = instruction ELSE i2 = instruction { If(c, [i1], [i2]) } 
    (* if(c) i*)
    | IF LPAR c=expression RPAR i = instruction  { If(c, [i], []) } 
    (* if(c);*)
    | IF LPAR c=expression RPAR SEMI { If(c, [], []) } 
  (* While *)
    (* While(c){s} *)
    | WHILE LPAR c=expression RPAR BEGIN s = list(instruction) END { While(c,s) }
    (* While(c); *)
    | WHILE LPAR c=expression RPAR SEMI { While(c, []) }
  (* Decls and sets *)
    | decl = variable_decl { Set(decl) }
    | PUTCHAR LPAR e=expression RPAR SEMI {Putchar(e)} 
  (* Scope creation : { s } *)
    | BEGIN s = list(instruction) END { Scope(s) }
;

call_list:
  | { [] }
  | e=expression { [e] } 
  | e=expression SEPARATOR cl=call_list { e::cl } 

expression:
  | f=IDENT LPAR p=call_list RPAR {Call(f, p)} 
  | n=CST { Cst(n) }
  | b=BOOL_CST { BCst(b) }
  | i=IDENT { Get(i) }
  | LPAR e=expression RPAR { e }
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
