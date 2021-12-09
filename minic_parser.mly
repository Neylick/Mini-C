%{
  open Lexing
  open Minic_ast
  
  let get_line_col_from_pos pos = (pos.pos_lnum, (pos.pos_cnum - pos.pos_bol))

  let associate_line_char_info node pos =
    let l,c = (get_line_col_from_pos pos) in 
    let pos_str = Printf.sprintf "line %d:%d" l c in
    (node, pos_str)
  
  
%}

(* Déclaration des lexèmes *)

%token <int> CST
%token <bool> BOOL_CST
%token <string> IDENT

%token WHILE DO FOR
%token IF ELSE SWITCH CASE DEFAULT
%token BREAK CONTINUE 
%token RETURN SET SEMI SEPARATOR DOTS2
%token LPAR RPAR BEGIN END

%left RPAR
%left SEMI
%right ELSE
%left CASE DOTS2

%token EOF

%token PUTCHAR
%token INT BOOL VOID


(* Grouper pour le type checker *)
%token LT GT LET GET EQ
%left LT GT LET GET EQ
(* Grouper pour le type checker *)
%token AND OR BAND BOR XOR BXOR NEQ BNEQ
%left AND OR BAND BOR XOR BXOR NEQ BNEQ
(* Grouper pour le type checker *)
%token ADD MUL DIV MOD SUB
%left ADD MUL DIV MOD SUB
(* Grouper pour le type checker *)
%token NOT BNOT
%left NOT BNOT

%start program

%type <Minic_ast.instr> instruction
%type <Minic_ast.expr> expression
%type <Minic_ast._expr> _expression
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

_expression:
  | f=IDENT LPAR p=funcall_args RPAR {Call(f, p)} 
  | n=CST { Cst(n) }
  | b=BOOL_CST { BCst(b) }
  | i=IDENT { Get(i) }
  | LPAR e=expression RPAR { let _e, _ = e in _e }
  | bop=bool_op { bop }
  | iop=int_op { iop }
  | bitop=bitwise_op { bitop }
;

expression: e=_expression { (associate_line_char_info e $startpos) } ;

_instruction:
  | l=loop { l }
  | c=conditional { c }
  | r=return { r }
  | decl = variable_decl_set { Set(decl) }
  | BEGIN s = list(instruction) END { Scope(s) }
  | PUTCHAR LPAR e=expression RPAR SEMI {Putchar(e)} 
  | CONTINUE SEMI { Continue }
  | BREAK SEMI { Break }
;

instruction: i = _instruction { (associate_line_char_info i $startpos) } ;


global_scope_def_list:
  | { [] }
  | d = global_scope_def l=global_scope_def_list { d::l }
;

global_scope_def:
  | vd = variable_decl_set { let v, i = associate_line_char_info vd $startpos in Variable(v,i) }
  | fd = function_decl { let _, info = associate_line_char_info (fd) $startpos in Function(fd, info) }
;

typ:
  | INT { (Int) }
  | BOOL { (Bool) }
  | VOID { (Void) }
;

simple_var_decl:
  | t = typ i = IDENT { (t, i, (Undef, "err")) }
;

variable_set:
  (* <T> ident = val *)
    | t = typ i = IDENT SET v = expression { (t, i, v) }
  (* ident = val *)
    | i = IDENT SET v = expression { (None, i, v) }
  (* ident ++ *)
    | i = IDENT ADD ADD { (None, i, (Add((Get(i),"err"), (Cst(1),"err")), "err")) }
    | ADD ADD i = IDENT { (None, i, (Add((Get(i),"err"), (Cst(1),"err")), "err")) }
  (* ident -- *)
    | i = IDENT SUB SUB { (None, i, (Sub((Get(i),"err"), (Cst(1),"err")), "err")) }
    | SUB SUB i = IDENT { (None, i, (Sub((Get(i),"err"), (Cst(1),"err")), "err")) }
;

variable_decl_set:
  | set = variable_set SEMI { set }
  | decl = simple_var_decl SEMI { decl }
;

parameter_list:
  | { [] }
  | d=simple_var_decl { let (t, i, v) = d in let _, info = v in [((t, i), info)] } (* No separator in that case *)
  | d=simple_var_decl SEPARATOR p=parameter_list { let (t, i, v) = d in let _, info = v in ((t, i), info)::p }
;

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
  | set=variable_set {[( associate_line_char_info (Set(set)) $startpos )]}
  | set=variable_set SEPARATOR fs = for_seq {( associate_line_char_info (Set(set)) $startpos )::fs}
;

expr_case_list:
  | CASE e=expression DOTS2 { [e] }
  | CASE e=expression DOTS2 ecl=expr_case_list { e::ecl }
;

block_case_list :
  | { [] }
  | ecl=expr_case_list s=list(instruction) cl=block_case_list {(ecl,s)::cl}
;

loop:
  (* While *)
    (* While(c) i *)
    | WHILE LPAR c=expression RPAR i=instruction { While(c, i) }
    (* While(c); *)
    | WHILE LPAR c=expression RPAR SEMI { While(c, (Skip, "")) }
  (* Do While *)
    (* do{ s } while( c ); *)
    | DO BEGIN i = instruction END WHILE LPAR c=expression RPAR SEMI { DoWhile(i,c) }
  (* For *)
    | FOR LPAR init_instr=for_seq SEMI cond = expression SEMI incr_instr=for_seq RPAR BEGIN i = instruction END { For(init_instr, cond, incr_instr, i) } 
    | FOR LPAR init_instr=for_seq SEMI SEMI incr_instr=for_seq RPAR BEGIN i = instruction END { For(init_instr, (BCst(true),"err"), incr_instr, i) } 
;

conditional:
  (* If *)
    (* if(c) i1 else i2*)
    | IF LPAR c=expression RPAR i1 = instruction ELSE i2 = instruction { If(c, i1, i2) } 
    (* if(c) i*)
    | IF LPAR c=expression RPAR i = instruction  { If(c, i, (Skip, "err")) } 
    (* if(c);*)
    | IF LPAR c=expression RPAR SEMI { If(c, (Skip, "err"), (Skip, "err")) } 
  (* Switch *)
    | SWITCH LPAR e=expression RPAR BEGIN cl=block_case_list DEFAULT DOTS2 def=list(instruction) END { Switch(e, cl, def) }
    | SWITCH LPAR e=expression RPAR BEGIN cl=block_case_list END { Switch(e, cl, []) }
;


return:
  (* return _; *)
    | RETURN e=expression SEMI { Return e }
  (* return(_);*)
    | RETURN LPAR e=expression RPAR SEMI { Return e }
  (* return;*)
    | RETURN SEMI { Return(Undef, "err") }
;


funcall_args:
  | { [] }
  | e=expression { [e] } 
  | e=expression SEPARATOR cl=funcall_args { e::cl } 
;

bool_op:
  | NOT a=expression {Not(a)}
  | a=expression LT b=expression  {Lt(a,b)} 
  | a=expression GT b=expression  {Gt(a,b)}
  | a=expression LET b=expression  {Leqt(a,b)}
  | a=expression GET b=expression  {Geqt(a,b)}
  | a=expression EQ b=expression  {Eq(a,b)}

  | a=expression NEQ b=expression  {Neq(a,b)}
  | a=expression AND b=expression  {And(a,b)}
  | a=expression OR b=expression  {Or(a,b)}
  | a=expression XOR b=expression  {Xor(a,b)}
;

int_op:
  | a=expression ADD b=expression  {Add(a,b)}
  | a=expression MUL b=expression  {Mul(a,b)}
  | a=expression DIV b=expression  {Div(a,b)}
  | a=expression MOD b=expression  {Mod(a,b)}
  | a=expression SUB b=expression  {Sub(a,b)}
;

bitwise_op:
  | BNOT a=expression {BNot(a)}
  | a=expression BNEQ b=expression  {BNeq(a,b)}
  | a=expression BAND b=expression  {BAnd(a,b)}
  | a=expression BOR b=expression  {BOr(a,b)}
  | a=expression BXOR b=expression  {BXor(a,b)}
;


