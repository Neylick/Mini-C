open Minic_ast
module Env = Map.Make(String)

(* Limitation on variables for each scope *)


let typecheck_program (prog: prog) =  
  (* 
    TODO
    - Verifier le bon typage des globals.
    - Verifier la bonne definition en comparant avec parametres, globals, locals
    - Verifier les types des op
  *)
  let global_env = Hashtbl.create 100 in 
  let _ = List.iter (fun (t, i, v) -> Hashtbl.add global_env i (t, v)) prog.globals in

  let typecheck_function (fdef: fun_def) =
    let local_env = Hashtbl.create 100 in
    let _ = List.iter (fun (t, i) -> Hashtbl.add local_env i (t, Undef)) fdef.params in

    let rec type_expr = function
      | Cst _ -> Int
      | BCst _ -> Bool
      | Undef -> Void
      | Add(e1, e2) | Mul (e1, e2) |Div (e1, e2) |Mod (e1, e2) |Sub (e1, e2) -> Int
      | Lt (e1, e2) | Gt (e1, e2) | Leqt (e1, e2) |Geqt (e1, e2) |Eq (e1, e2) |Neq (e1, e2) |Or (e1, e2) |And (e1, e2) |Xor (e1, e2) -> Bool
      | BNeq (e1, e2) |BOr (e1, e2) |BAnd (e1, e2) | BXor (e1, e2) -> Bool
      | Get i ->  let errmsg = "Undefined variable : "^i in
        begin 
          try let t, _ = Hashtbl.find local_env i in t
          with Not_found -> 
            try let t, _ = Hashtbl.find global_env i in t
            with Not_found -> failwith errmsg
        end
      | Call (name, params) ->  let f = List.find (fun f -> (f.name = name)) prog.functions in 
                                List.iter2 (fun p (t, _) -> if type_expr p <> t then failwith "Parameter type unmatched : error") params f.params;
                                f.return
      
    in
    let rec typecheck_instr = function
      | Return e -> let t = type_expr e in if t <> fdef.return then failwith "Return type doesn't match function declaration : error"
      | Putchar c ->  let t = type_expr c in if t <> Int then failwith "Non integer type given to putchar : error"
      | Set (t, i, Undef) -> Hashtbl.add local_env i (t, Undef) 
      | Set (None, i, v) -> let errmsg = "Undefined variable : "^i^" at line " in 
        begin
          try let t, _ = Hashtbl.find global_env i in let te = type_expr v in  
            if t <> te  then failwith "Value can't match declared type : error"
                        else Hashtbl.add global_env i (t, v) 
          with Not_found -> 
            try let t, _ = Hashtbl.find local_env i in let te = type_expr v in 
              if t <> te  then failwith "Value can't match declared type : error"
              else Hashtbl.add local_env i (t, v) 
            with Not_found -> failwith errmsg
        end
      | Set (t, i, v) ->  let te = type_expr v in if t <> te  then failwith "Value can't match declared type : error"
                                                              else Hashtbl.add local_env i (t, v) 
      | If(c, s1, s2) -> let t = type_expr c in
                        if t <> Bool then failwith "Non boolean condition : error";
                        typecheck_seq s1;
                        typecheck_seq s2;
      | While(c, s) ->  let t = type_expr c in
                        if t <> Bool then failwith "Non boolean condition : error";
                        typecheck_seq s;
      | Skip _ -> ()

    and typecheck_seq s = List.iter typecheck_instr s in
    typecheck_seq (fdef.code);
  in
  List.iter typecheck_function (prog.functions)