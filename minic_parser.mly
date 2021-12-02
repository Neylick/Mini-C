%{
  open Lexing
  open Minic_ast
%}

(* Déclaration des lexèmes *)

%token <int> CST
%token <bool> BOOL_CST
%token <string> IDENT

%token WHILE IF ELSE DO FOR

%token SWITCH CASE DEFAULT

%token BREAK CONTINUE 

%token LPAR RPAR BEGIN END
%token RETURN SET SEMI SEPARATOR DOTS2
%token EOF

%token PUTCHAR
%token INT BOOL VOID

(* Grouper pour le type checker *)
%token LT GT LET GET EQ
(* Grouper pour le type checker *)
%token AND OR BAND BOR XOR BXOR NEQ BNEQ
(* Grouper pour le type checker *)
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
  | vd = variable_decl_set {Variable(vd)}
  | fd = function_decl {Function(fd)}
;

typ:
  | INT { Int }
  | BOOL { Bool }
  | VOID { Void }
;

(* 
  Just a var decl, <type ident> 
  ****** NO SEMICOLON ****** 
*)
simple_var_decl:
  | t = typ i = IDENT { (t, i, Undef) }


variable_set:
  (* <T> ident = val *)
    | t = typ i = IDENT SET v = expression { (t, i, v) }
  (* ident = val *)
    | i = IDENT SET v = expression { (None, i, v) }
  (* ident ++ *)
    | i=IDENT ADD ADD { (None, i, Add(Get(i),Cst(1))) }
    | ADD ADD i=IDENT { (None, i, Add(Get(i),Cst(1))) }
  (* ident -- *)
    | i=IDENT SUB SUB { (None, i, Add(Get(i),Cst(1))) }
    | SUB SUB i=IDENT { (None, i, Add(Get(i),Cst(1))) }
;

(* Decls and sets *)
variable_decl_set:
  | set = variable_set SEMI { set }
  | decl = simple_var_decl SEMI { decl }
;

(* Parameters in function *)
parameter_list:
  | { [] }
  | d=simple_var_decl { let (t, i, _) = d in [(t,i)] } (* No separator in that case *)
  | d=simple_var_decl SEPARATOR p=parameter_list { let (t, i, _) = d in (t,i)::p }
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

for_seq:
  | { [] }
  | set=variable_set {[Set(set)]}
  | set=variable_set SEPARATOR fs = for_seq {Set(set)::fs}

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
  (* Do While *)
    (* do{ s } while( c ); *)
    | DO BEGIN s = list(instruction) END WHILE LPAR c=expression RPAR SEMI { DoWhile(s,c) }
  (* For *)
    | FOR LPAR init_instr=for_seq SEMI cond = expression SEMI incr_instr=for_seq RPAR BEGIN s = list(instruction) END { For(init_instr, cond, incr_instr, s) } 
    | FOR LPAR init_instr=for_seq SEMI SEMI incr_instr=for_seq RPAR BEGIN s = list(instruction) END { For(init_instr, BCst(true), incr_instr, s) } 
  (* Decls and sets *)
    | decl = variable_decl_set { Set(decl) }
  (* Scope creation : { s } *)
    | BEGIN s = list(instruction) END { Scope(s) }
  (* Putchar *)
    | PUTCHAR LPAR e=expression RPAR SEMI {Putchar(e)} 
;

call_list:
  | { [] }
  | e=expression { [e] } 
  | e=expression SEPARATOR cl=call_list { e::cl } 
;

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
