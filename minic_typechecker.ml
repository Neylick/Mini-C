open Minic_ast

let rec find_return_instr = function
  | Return _ -> true
  | Scope s -> find_return_seq s
  | If(_, _, []) -> false
  | If(_, s1, s2) -> find_return_seq s1 && find_return_seq s2
  | While(_, s) ->  find_return_seq s
  | _ -> false
and find_return_seq s = List.fold_left (fun acc i -> find_return_instr i || acc) false s

let typecheck_program (prog: prog) =  
  (* 
    TODO :
     *
     *
     *

    (?) Ligne et colonne dans les messages d'erreur du type checker 
    (?) Convertir les oct en decimal manuellement
    (?) Return dans chaque path de fonction
    (?) Traduire en assembleur
  *)

  (* 
    On utilise des tables de hachage pour pouvoir ajouter et supprimer les variables
    en changeant de blocs, et changer les valeurs des variables a la volee
    
    Hashtb, Variables : ( name -> (type * name * expression) )
    Hashtb, Fonction  : ( name -> {name, code, params, return} ) 
  *)

  let global_env = Hashtbl.create 100 in
  let local_env = Hashtbl.create 100 in
  let function_list = Hashtbl.create 100 in

  let rec type_expr = function
    | Cst _ ->  Int
    | BCst _ -> Bool
    | Undef -> Void 
    | Param -> None 
    | Add(e1, e2) | Mul (e1, e2) |Div (e1, e2) |Mod (e1, e2) |Sub (e1, e2) -> 
      let t1 = type_expr e1 in 
      let t2 = type_expr e2 in
      if t1 <> t2 || t1 <> Int 
      then failwith "[error] Integer operand was given a non integer expression"
      else Int
    | Lt (e1, e2) | Gt (e1, e2) | Leqt (e1, e2) |Geqt (e1, e2) |Eq (e1, e2) |Neq (e1, e2) ->
      let t1 = type_expr e1 in 
      let t2 = type_expr e2 in 
      if t1 <> t2 || t1 <> Int 
      then failwith "[error] Integer operand was given a non integer expression"
      else Bool
    | Or (e1, e2) | And (e1, e2) | Xor (e1, e2) -> 
      let t1 = type_expr e1 in 
      let t2 = type_expr e2 in
      if t1 <> t2 || t1 <> Bool 
      then failwith "[error] Boolean operand was given a non boolean expression"
      else Bool
    (* Really depends on how we implement things temp : *)
    | BNeq (e1, e2) |BOr (e1, e2) |BAnd (e1, e2) | BXor (e1, e2) -> 
      let t1 = type_expr e1 in 
      let t2 = type_expr e2 in
      if t1 <> t2
      then failwith "[error] Operand was given two expression with different size"
      else Bool
    | Get i -> 
      begin
        try let t, v = Hashtbl.find local_env i in if v = Undef then failwith "[error] Variable not initialised" else t
        with Not_found -> 
          try let t, v = Hashtbl.find global_env i in 
          if v = Undef 
          then failwith "[error] Variable not initialised" 
          else t
        with Not_found -> failwith ("[error] Undefined variable \""^i^"\"")
      end
    | Call (name, params) ->  
      begin
        try let f = Hashtbl.find function_list name in 
        List.iter2 (fun p (t, _) -> if type_expr p <> t then failwith "[error] Parameter type unmatched") params f.params; f.return
        with Not_found -> failwith ("[error] Function "^name^" is not defined")
      end
  in 
  
  let typecheck_function (fdef: fun_def) =
    let rec typecheck_instr = function
      | Skip -> ()
      | Expr e -> let _ = type_expr e in ()
      | Return e -> let t = type_expr e in if t <> fdef.return then failwith "[error] Return type doesn't match function declaration"
      | Putchar c ->  let t = type_expr c in if t <> Int then failwith "[error] Non integer type given to putchar" 
      | Scope s ->
        begin (* Makes a copy of the local environment to keep only the previously defined variables*)
          let prev_env = Hashtbl.copy local_env in 
          typecheck_seq s;
          Hashtbl.iter (fun i _ -> if not (Hashtbl.mem prev_env i) then Hashtbl.remove local_env i) local_env;
        end
      | Set (t, i, Undef) -> Hashtbl.add local_env i (t, Undef)
      | Set (None, i, v) -> 
        begin
          try let t, _ = Hashtbl.find local_env i in let te = type_expr v in  
            if t <> te  
            then failwith "[error] Value can't match declared type"
            else Hashtbl.add local_env i (t, v)
          with Not_found -> 
            try let t, _ = Hashtbl.find global_env i in let te = type_expr v in 
              if t <> te  
              then failwith "[error] Value can't match declared type"
              else Hashtbl.add global_env i (t, v)
            with Not_found -> failwith ("[error] Undefined variable : "^i)
        end
      | Set (t, i, v) -> 
        if (Hashtbl.mem local_env i) ||  (Hashtbl.mem global_env i) 
        then failwith "[error] Variable declaration duplicate"
        else let te = type_expr v in if t <> te  then failwith "[error] Value can't match declared type"
                                  else Hashtbl.add local_env i (t, v) 
      | If(c, s1, s2) -> 
        let t = type_expr c in
        if t <> Bool then failwith "[error] Non boolean condition";
        typecheck_instr (Scope s1);
        typecheck_instr (Scope s2);
      | While(c, s) ->  
        let t = type_expr c in
        if t <> Bool then failwith "[error] Non boolean condition";
        typecheck_instr (Scope s);

    and typecheck_seq s = List.iter typecheck_instr s 
    in
    (* Typecheck function *)
      begin (* makes a copy of the local environment to keep only the previously defined variables*)
        let prev_env = Hashtbl.copy local_env in 
        let _ = List.iter (fun (t, i) -> Hashtbl.add local_env i (t, Param)) fdef.params in
        typecheck_seq (fdef.code);
        Hashtbl.iter (fun i _ -> if not (Hashtbl.mem prev_env i) then Hashtbl.remove local_env i) local_env;
      end
  in

  (* Only used for initial checks : exisiting main and maybe we could check some other reserved variables/functions... *)
    let functions, _ =
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
  then failwith "[error] Couldn't find main function (no entry point)";
  
  (* Doing the checks that way makes the timeline linear, instead of finding any global and any functions. *)
  List.iter 
    (
      fun def ->
      match def with
      | Variable(t,i,v) -> 
        (* 
          Assert that the variable value corresponds to type and there's no duplicate, 
          then add to globals list.
        *)
        if (Hashtbl.mem global_env i) 
        then failwith "[error] Variable declaration duplicate"
        else let te = type_expr v in 
          if t <> te  
          then failwith "[error] Value can't match declared type"
          else Hashtbl.add global_env i (t, v)
      | Function f ->  
        (* 
          Assert return exists in all function path then add the function to the list.
        *)
        if f.return <> Void && not (find_return_seq f.code)
        then failwith ("[error] Some of the paths of "^f.name^" don't have return")
        else Hashtbl.add function_list f.name f; 
        typecheck_function f
    )
  prog