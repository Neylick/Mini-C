open Minic_ast
module Env = Map.Make(String)

(* Limitation on variables for each scope *)

let rec find_return_instr = function
  | Return _ -> true
  | Scope s -> find_return_seq s
  | If(_, s1, s2) -> find_return_seq s1 || find_return_seq s2
  | While(_, s) ->  find_return_seq s
  | _ -> false
and find_return_seq s = List.fold_left (fun acc i -> find_return_instr i || acc) false s

let typecheck_program (prog: prog) =  
  (* 
    TODO
    - Verifier le bon typage des globals.
    - Putchar 
    - Return dans chaque path de fonction
    - Traduire en assembleur ?
  *)
  let global_env = Hashtbl.create 100 in
  let function_list = Hashtbl.create 100 in 

  let typecheck_function (fdef: fun_def) =
    (* Return exists *)
    if fdef.return <> Void && fdef.name <> "main"
    then  if not (find_return_seq fdef.code)
          then failwith "Couldn't find return in non void and non main function : error";
    let local_env = Hashtbl.create 100 in
    let _ = List.iter (fun (t, i) -> Hashtbl.add local_env i (t, Param)) fdef.params in

    let rec type_expr = function
      | Cst _ -> Int
      | BCst _ -> Bool
      | Undef -> Void (* Check that for non initialized var and return types *)
      | Param -> None (* We should'nt ever need to check that, if we do we should get an error *)
      | Add(e1, e2) | Mul (e1, e2) |Div (e1, e2) |Mod (e1, e2) |Sub (e1, e2) -> 
        let t1 = type_expr e1 in let t2 = type_expr e2 in
        if t1 <> t2 || t1 <> Int 
        then failwith "Integer operand was given a non integer expression : error"
        else Int
      | Lt (e1, e2) | Gt (e1, e2) | Leqt (e1, e2) |Geqt (e1, e2) |Eq (e1, e2) |Neq (e1, e2) ->
        let t1 = type_expr e1 in let t2 = type_expr e2 in 
        if t1 <> t2 || t1 <> Int 
        then failwith "Integer operand was given a non integer expression : error"
        else Bool
      | Or (e1, e2) | And (e1, e2) | Xor (e1, e2) -> 
        let t1 = type_expr e1 in let t2 = type_expr e2 in
        if t1 <> t2 || t1 <> Bool 
        then failwith "Boolean operand was given a non boolean expression : error"
        else Bool
        (* Really depends on how we implement things temp : *)
        | BNeq (e1, e2) |BOr (e1, e2) |BAnd (e1, e2) | BXor (e1, e2) -> 
          let t1 = type_expr e1 in let t2 = type_expr e2 in
          Bool 
      | Get i ->  let errmsg = "Undefined variable \""^i^"\" : error" in
        begin 
          try let t, v = Hashtbl.find local_env i in if v = Undef then failwith "Variable not initialised : error " else t
          with Not_found -> 
            try let t, v = Hashtbl.find global_env i in 
              if v = Undef 
              then failwith "Variable not initialised : error " 
              else t
            with Not_found -> failwith errmsg
        end
      | Call (name, params) ->  try let f = Hashtbl.find function_list name in 
                                  begin
                                    List.iter2 (fun p (t, _) -> if type_expr p <> t then failwith "Parameter type unmatched : error") params f.params; f.return
                                  end
                                with Not_found -> failwith ("Function "^name^" is not defined")
    in
    let rec typecheck_instr = function
      | Skip -> ()
      | Scope s ->
        begin (* makes a copy of the local environment to keep only the previously defined variables*)
          let prev_env = Hashtbl.copy local_env in 
          typecheck_seq s;
          Hashtbl.iter (fun i _ -> if not (Hashtbl.mem prev_env i) then Hashtbl.remove local_env i) local_env;
        end
      | Return e -> let t = type_expr e in if t <> fdef.return then failwith "Return type doesn't match function declaration : error"
      | Putchar c ->  let t = type_expr c in if t <> Int then failwith "Non integer type given to putchar : error"
      | Set (t, i, Undef) -> Hashtbl.add local_env i (t, Undef) 
      | Set (None, i, v) -> let errmsg = "Undefined variable : "^i in 
        begin
          try let t, _ = Hashtbl.find local_env i in let te = type_expr v in  
            if t <> te  
            then failwith "Value can't match declared type : error"
            else Hashtbl.add local_env i (t, v)
          with Not_found -> 
            try let t, _ = Hashtbl.find global_env i in let te = type_expr v in 
              if t <> te  
              then failwith "Value can't match declared type : error"
              else Hashtbl.add global_env i (t, v)
            with Not_found -> failwith errmsg
        end
      | Set (t, i, v) -> 
        if (Hashtbl.mem local_env i) ||  (Hashtbl.mem global_env i) 
        then failwith "Variable declaration duplicate : error"
        else let te = type_expr v in if t <> te  then failwith "Value can't match declared type : error"
                                  else Hashtbl.add local_env i (t, v) 
      | If(c, s1, s2) -> let t = type_expr c in
                        if t <> Bool then failwith "Non boolean condition : error";
                        typecheck_instr (Scope s1);
                        typecheck_instr (Scope s2);
      | While(c, s) ->  let t = type_expr c in
                        if t <> Bool then failwith "Non boolean condition : error";
                        typecheck_instr (Scope s);

    and typecheck_seq s = List.iter typecheck_instr s in
    typecheck_seq (fdef.code);
  in

  (* Used for initial checks : exisiting main and maybe we could check some reserved globals or things like that *)
  let functions, globals =
    List.fold_right
      (
        fun def acc ->
        match def with
        | Variable(t,i,v) -> let (fs,vs) = acc in (fs, (t,i,v)::vs) 
        | Function f -> let (fs,vs) = acc in (f::fs, vs)
      )
      prog ([],[])
  in 

  if not (List.fold_left (fun acc f -> if f.name = "main" then true else acc) false functions) 
  then failwith "Couldn't find main function (no entry point) : error";

  let _ = List.iter (fun (t, i, v) -> 
                      Hashtbl.add global_env i (t, v)
                    ) 
          globals in

  (* List.iter typecheck_function functions *)
  List.iter 
    (
      fun def ->
      match def with
      | Variable(t,i,v) -> Hashtbl.add global_env i (t, v)
      | Function f -> Hashtbl.add function_list f.name f; typecheck_function f
    )
  prog