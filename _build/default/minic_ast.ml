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
  | Cst of int (* 0 *)
  | BCst of bool (* true *)
  | Undef
  | Param

(* Représentation des instructions et séquences. *)
type instr =
  | Putchar of expr (* putchar(n); *)
  | Set of (typ * string * expr)
  | If of expr * seq * seq (* if (c) { s1 } else { s2 } *)
  | While of expr * seq (* while (c) { s } *)
  | Scope of seq (* { s } *)
  | Return of expr (* return v; *)
  | Skip (* *)
and seq = instr list

(* Représentétion des fonctions. *)
type fun_def = 
{
  name: string;
  params: (typ * string) list;
  return: typ;
  code: seq;
}

(* Représentation des programmes.
   En réponse à l'indication de l'énoncé, j'associe une valeur entière
   à chaque variable globale. Mais vous voudrez peut-être faire évoluer
   cela (et procéder de même pour les variables locales des fonctions). *)

type prog = 
{
  globals: (typ * string * expr) list;
  functions: fun_def list;
}

