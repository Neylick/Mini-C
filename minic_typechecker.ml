open Minic_ast

let init_env_size = 0

let keepleft listOtuples = let (l, _) = List.split listOtuples in l
let keepright listOtuples = let (_, r) = List.split listOtuples in r

let rec find_return_instr i = 
  let instr, _ = i in
  match instr with
  | Return _ -> true
  | Scope s -> find_return_seq s
  | If(_, _, (Skip, _)) -> false
  | If(_, i1, i2) -> find_return_instr i1 && find_return_instr i2
  | While(_, i) -> find_return_instr i
  | Switch(_, cl, def) -> 
    (find_return_seq def) && 
    (List.fold_left (fun acc (_, seq) -> (find_return_seq seq) && acc) true cl)
  | _ -> false
and find_return_seq s = List.fold_left (fun acc i -> find_return_instr i || acc) false s

let typecheck_program (prog: prog) =  
  (* 
    On utilise des tables de hachage pour pouvoir ajouter et supprimer les variables
    en changeant de blocs, et changer les valeurs des variables a la volee
    
    Hashtb, Variables : ( name -> (type * name * expression) * string )
    Hashtb, Fonction  : ( name -> {name, code, params, return} * string ) 
  *)

  let global_env = Hashtbl.create init_env_size in
  let local_env = Hashtbl.create init_env_size in
  let function_list = Hashtbl.create init_env_size in

  let rec type_expr e = 
    let expr, infos = e in
    match expr with 
    | Cst _ ->  Int
    | BCst _ -> Bool
    | Undef -> None
    | Param -> None 
    | Not b -> let t = type_expr b in if t <> Bool then failwith ("<"^infos^">[error] Boolean operand was given a non boolean expression") else Bool
    | BNot b -> type_expr b
    | Add(e1, e2) | Mul (e1, e2) |Div (e1, e2) |Mod (e1, e2) |Sub (e1, e2) -> 
      let t1 = type_expr e1 in 
      let t2 = type_expr e2 in
      if t1 <> t2 || t1 <> Int 
      then failwith ("<"^infos^">[error] Integer operand was given a non integer expression")
      else Int
    | Lt (e1, e2) | Gt (e1, e2) | Leqt (e1, e2) |Geqt (e1, e2) |Eq (e1, e2) |Neq (e1, e2) ->
      let t1 = type_expr e1 in 
      let t2 = type_expr e2 in 
      if t1 <> t2 || t1 <> Int 
      then failwith ("<"^infos^">[error] Integer operand was given a non integer expression")
      else Bool
    | Or (e1, e2) | And (e1, e2) | Xor (e1, e2) -> 
      let t1 = type_expr e1 in 
      let t2 = type_expr e2 in
      if t1 <> t2 || t1 <> Bool 
      then failwith ("<"^infos^">[error] Boolean operand was given a non boolean expression")
      else Bool
    | BNeq (e1, e2) |BOr (e1, e2) |BAnd (e1, e2) | BXor (e1, e2) -> 
      let t1 = type_expr e1 in 
      let t2 = type_expr e2 in
      if t1 <> t2
      then failwith ("<"^infos^">[error] Operand was given two expression with different size")
      else t1
    | Get i -> 
      begin
        try let t, v, _ = Hashtbl.find local_env i in if v = Undef then failwith ("<"^infos^">[error] Variable \""^i^"\" was not initialised") else t
        with Not_found -> 
          try let t, v, _= Hashtbl.find global_env i in 
          if v = Undef 
          then failwith ("<"^infos^">[error] Variable \""^i^"\" was not initialised") 
          else t
        with Not_found -> failwith ("<"^infos^">[error] Undefined variable \""^i^"\"")
      end
    | Call (name, params) ->  
      begin
        try let f = Hashtbl.find function_list name in 
          try List.iter2 (fun p (pdef, pinfo) -> let t,_ = pdef in if type_expr p <> t then failwith ("{"^pinfo^"}[error] Function \""^name^"\": parameter type mismatch.") ) params f.params; f.return
          with Invalid_argument _ -> failwith ("<"^infos^">[error] Function \""^name^"\" expects "^(string_of_int (List.length f.params))^" parameters but was given "^(string_of_int (List.length params))^".")
        with Not_found -> failwith ("<"^infos^">[error] Function \""^name^"\" is not defined")
      end
  in 
  
  let typecheck_function (fdef: fun_def) =
    let rec typecheck_instr i =
      let instr, infos = i in 
      match instr with
      | Skip -> ()
      | Break -> ()
      | Continue -> ()
      | Expr e -> let _ = type_expr e in ()
      | Putchar c ->  let t = type_expr c in if t <> Int then failwith ("<"^infos^">[error] Non integer type given to putchar.")
      | Return e -> let t = type_expr e in if t <> fdef.return then failwith ("<"^infos^">[error] Function \""^fdef.name^"\": return type doesn't match function declaration.")
      | Scope s ->
        begin (* Makes a copy of the local environment to keep only the previously defined variables*)
          let prev_env = Hashtbl.copy local_env in 
          typecheck_seq s;
          Hashtbl.iter (fun i _ -> if not (Hashtbl.mem prev_env i) then Hashtbl.remove local_env i) local_env;
        end
      | Set (None, id, e) -> 
        begin
          try let t, _, varinfo = Hashtbl.find local_env id in let te = type_expr e in  
            if t <> te  
            then failwith ("<"^infos^">[error] \""^id^"\" : value can't match declared type.")
            else Hashtbl.replace local_env id (t, fst e , varinfo) 
          with Not_found -> 
            try let t, _, varinfo = Hashtbl.find global_env id in let te = type_expr e in 
              if t <> te  
              then failwith ("<"^infos^">[error] \""^id^"\" : value can't match declared type.")
              else Hashtbl.replace global_env id (t, fst e , varinfo) 
            with Not_found -> failwith ("<"^infos^">[error] Undefined variable \""^id^"\"")
        end
      | Set (t, id, e) -> 
        if (Hashtbl.mem local_env id) ||  (Hashtbl.mem global_env id) 
        then failwith ("<"^infos^">[error] \""^id^"\" : variable declaration duplicate.")
        else let te = type_expr e in 
          if te <> None && t <> te (* Is initialized and has the good type *)
          then failwith ("<"^infos^">[error] \""^id^"\" : value can't match declared type.")
          else Hashtbl.add local_env id (t, fst e , snd e) 
      | If(e, i1, i2) -> 
        let t = type_expr e in
        if t <> Bool then failwith ("<"^infos^">[error] Non boolean condition in if statement.")
        else begin typecheck_instr i1; typecheck_instr i2; end
      | While(e, i) ->  
        let t = type_expr e in
        if t <> Bool 
        then failwith ("<"^infos^">[error] Non boolean condition in while loop.")
        else typecheck_instr i
      | DoWhile(i, e) -> typecheck_seq [i ; (While(e, i), infos)];
      | For(s1, e, s2, block) ->
        begin (* Makes a separate scope for our for loop*)
        let prev_env = Hashtbl.copy local_env in 
          typecheck_seq s1; (* The incr seq needs to impact the loop and incrementation seq *)
          let t = type_expr e in
          if t <> Bool 
          then failwith ("<"^infos^">[error] Non boolean condition in for loop.")
          else 
          begin
            typecheck_instr (Scope s2, "");
            typecheck_instr block;
            Hashtbl.iter 
            (
              fun i _ -> 
                if not (Hashtbl.mem prev_env i) 
                then Hashtbl.remove local_env i
            ) 
            local_env;
          end
        end (* Going out of the for loop *)
      | Switch(e, case_list, def) -> 
        (* 
          Here we create a scope because we don't have a break notion in the typecheck, 
          we can't do that in a compiler 
        *)
        begin
          let t1 = type_expr e in
          begin
            List.iter 
              (
                fun (comp_list, seq) ->
                  List.iter 
                    (
                      fun e_case -> 
                      if t1 <> (type_expr e_case)
                      then failwith ("<"^infos^">[error] Case type mismatch.")
                    )
                  comp_list;
                  typecheck_instr (Scope seq, "")
              )
            case_list;
            typecheck_instr (Scope def, "")
          end
        end
    and typecheck_seq s = List.iter typecheck_instr s 
    in
    (* Typecheck function *)
      begin (* Makes a copy of the local environment to keep only the previously defined variables*)
        let prev_env = Hashtbl.copy local_env in 
        let _ = List.iter (fun (p, pinfos) -> let pt, pid = p in Hashtbl.add local_env pid (pt, Param, pinfos)) fdef.params in
        typecheck_seq (fdef.code);
        Hashtbl.iter (fun i _ -> if not (Hashtbl.mem prev_env i) then Hashtbl.remove local_env i) local_env;
      end
  in
  if not 
    (List.fold_left 
      (
        fun acc decl ->
        match decl with 
        | Function (f, _) -> if f.name = "main" then true else acc
        | _ -> acc (* skip variables *)
      ) 
    false prog) 
  then failwith ("[error] Couldn't find main function (no entry point).");
  
  (* Doing the checks this way makes the file linear, instead of finding any global and any functions. *)
  List.iter 
    (
      fun def ->
      match def with
      | Variable(v, infos) -> 
        (* 
          Assert that the variable value corresponds to type and there's no duplicate, 
          then add to globals list.
        *)
        let t,i,e = v in 
        if (Hashtbl.mem global_env i) 
        then failwith ("<"^infos^">[error] \""^i^"\" : variable declaration duplicate.")
        else let te = type_expr e in 
          if t <> te  
          then failwith ("<"^infos^">[error] \""^i^"\" : value can't match declared type.")
          else Hashtbl.add global_env i (t, fst e, snd e)
      | Function(f, infos) ->  
        (* 
          Assert return exists in all function path then add the function to the list.
          In C, this is just a warning, maybe do the same here ?
          We also check the function code here.
        *)
        
        if f.return <> Void && not (find_return_seq f.code)
        then failwith ("<"^infos^">[error] Some of the paths of "^f.name^" don't have return.")
        else Hashtbl.add function_list f.name f; 
        typecheck_function f
    )
  prog