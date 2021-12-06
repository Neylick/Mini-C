(*Représentation des types.*)
type typ =
  | Int
  | Bool
  | Void
  | None

(* Représentation des expressions.*)
type expr =
  | Get of string
  | Call of string * expr list  (* f(a,b,...) *)
  (* Ops *)
    | Not of expr (* ! *)
    | BNot of expr (* ~ *)

    | Add of expr * expr (* + *)
    | Mul of expr * expr (* * *)
    | Div  of expr * expr (* / *)
    | Mod  of expr * expr (* % *)
    | Sub  of expr * expr (* - *)
  
    | Lt  of expr * expr (* < *)
    | Gt  of expr * expr (* > *)
    | Leqt  of expr * expr (* <= *)
    | Geqt  of expr * expr (* >= *)
    | Eq  of expr * expr (* == *)
  
    | Neq  of expr * expr (* != *)
    | BNeq  of expr * expr (* ~ *)
    | Or  of expr * expr (* || *)
    | BOr  of expr * expr (* | *)
    | And  of expr * expr (* && *)
    | BAnd  of expr * expr (* & *)
    | Xor  of expr * expr (* ^= *)
    | BXor  of expr * expr (* ^ *)
  | Cst of int 
  | BCst of bool 
  | Undef (* Utilise pour les variables void et non initialisees *)
  | Param (* Utilise pour les parametres, mais directement passe dans le pattern matching (sauf erreur) *)

(* Représentation des instructions et séquences. *)
type instr =
  | Putchar of expr (* putchar(n); *)
  | Set of (typ * string * expr) (* x = 0 *)
  | If of expr * instr * instr (* if (c) { s1 } else { s2 } *)
  | While of expr * instr (* while (c) { s } *)
  | DoWhile of instr * expr (* do { s } while (c) *)
  | For of seq * expr * seq * instr (* for(s1; c; s2){ s } *)
  | Switch of expr*(((expr list) * seq) list)*seq  (* switch(e) case a : ... break; default : .... break; *)
  | Scope of seq (* { s } *)
  | Return of expr (* return v; *)
  | Expr of expr
  | Break (* break; *)
  | Continue (* continue; *)
  | Skip (* *)
and seq = instr list

(* Représentétion des fonctions. *)
(* 
  Je n'utilise pas de local parce qu'ils n'ont aucun impact hors du code de la fonction. 
  (Les verifications se font donc en interne)
*)
type fun_def = 
{
  name: string;
  params: (typ * string) list;
  return: typ;
  code: seq;
}

(* 
  Representation des definitions dans le programmes :
  - Fonction definies comme des fun_def (voir au dessus)
  - Variables globales definies comme des associations de type (typ), identifiant (string) et valeur (expression)
  Je ne separe pas les deux dans le programme pour savoir quand ces definitions apparaissent.
*)
type global_scope_def =
  | Function of fun_def
  | Variable of (typ * string * expr)
  
(* Représentation des programmes. *)
type prog = global_scope_def list