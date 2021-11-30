open Minic_ast
module Env = Map.Make(String)

let typecheck_program (prog: prog) = 
  let global_env =
    List.fold_left (fun env (x, ty, _) -> Env.add x ty env) Env.empty prog.globals
  in  
  let typecheck_function (fdef: fun_def) =
    (* TODO *)
    let rec type_expr = function
      | Cst _ -> Int
      | BCst _ -> Bool
      (* TODO : Type check operators *)
      | Add(e1, e2)| Mul (e1, e2)|Div (e1, e2)|Mod (e1, e2)|Sub (e1, e2) -> Int
      | Lt (e1, e2)| Gt (e1, e2)| Leqt (e1, e2)|Geqt (e1, e2)|Eq (e1, e2)|Neq (e1, e2)|BNeq (e1, e2)|Or (e1, e2)| BOr (e1, e2)|And (e1, e2)| BAnd (e1, e2)| Xor (e1, e2)| BXor (e1, e2) -> Bool
      | Get i -> let errmsg = "undefined variable : "^i in if not (Env.mem i global_env) then failwith errmsg; Env.find i global_env
      | Call (name, params) -> let f = List.find (fun f -> (f.name = name)) prog.functions in 
      begin
        List.iter2 (fun p (_, t) -> if type_expr p <> t then failwith "Parameter type unmatched : error") params f.params;
        f.return
      end
    in
    let rec typecheck_instr = function
      | Return(e) -> let t = type_expr e in if t <> fdef.return then failwith "Return type doesn't match : error"
      | If(c, s1, s2) -> let t = type_expr c in
                        if t <> Bool then failwith "Non boolean condition : error";
                        typecheck_seq s1;
                        typecheck_seq s2;
      | While(c, s) ->  let t = type_expr c in
                        if t <> Bool then failwith "Non boolean condition : error";
                        typecheck_seq s;

      | Putchar c ->  let t = type_expr c in if t <> Int then failwith "Non integer type given to putchar : error"
      | Set (_, _) -> () (* TODO *)
      | Expr _ -> () (* TODO *) 

    (* TODO *)
    and typecheck_seq s =
      List.iter typecheck_instr s        
    in
    typecheck_seq (fdef.code);
  in
    
(* 
  TODO
  Verifier le bon typage des globals.
*)  
  List.iter typecheck_function (prog.functions);