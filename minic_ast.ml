(* Représentation des programmes.
   En réponse à l'indication de l'énoncé, j'associe une valeur entière
   à chaque variable globale. Mais vous voudrez peut-être faire évoluer
   cela (et procéder de même pour les variables locales des fonctions). *)
type prog = 
{
  globals: (string * typ * int) list;
  functions: fun_def list;
}

(* Représentation des types. *)
type typ =
  | Int
  | Bool
  | Void

(* Représentation des expressions.
   Ajouté : les constantes booléennes. *)
type expr =
  | Cst of int (* 0 *)
  | BCst of bool (* true *)
  (* Ops *)
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

  | Get of string
  | Call of string * expr list  (* f(a,b,...) *)

(* Représentation des instructions et séquences. *)
type instr =
  | Putchar of expr (* putchar(n); *)
  | Set of string * expr (* x = n; *)
  | If  of expr * seq * seq (* if (c) { s1 } else { s2 } *)
  | While of expr * seq (* while (c) { s } *)
  | Return of expr (* return(v); *)
  | Expr of expr (*  *)
and seq = instr list

(* Représentétion des fonctions. *)
type fun_def = 
{
  name: string;
  params: (string * typ) list;
  return: typ;
  locals: (string * typ) list;
  code: seq;
}


