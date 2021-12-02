
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | XOR
    | WHILE
    | VOID
    | SUB
    | SET
    | SEPARATOR
    | SEMI
    | RPAR
    | RETURN
    | PUTCHAR
    | OR
    | NEQ
    | MUL
    | MOD
    | LT
    | LPAR
    | LET
    | INT
    | IF
    | IDENT of (
# 10 "minic_parser.mly"
       (string)
# 30 "minic_parser.ml"
  )
    | GT
    | GET
    | EQ
    | EOF
    | END
    | ELSE
    | DIV
    | CST of (
# 8 "minic_parser.mly"
       (int)
# 42 "minic_parser.ml"
  )
    | BXOR
    | BOR
    | BOOL_CST of (
# 9 "minic_parser.mly"
       (bool)
# 49 "minic_parser.ml"
  )
    | BOOL
    | BNEQ
    | BEGIN
    | BAND
    | AND
    | ADD
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState123
  | MenhirState114
  | MenhirState110
  | MenhirState105
  | MenhirState97
  | MenhirState96
  | MenhirState94
  | MenhirState92
  | MenhirState87
  | MenhirState80
  | MenhirState78
  | MenhirState77
  | MenhirState73
  | MenhirState71
  | MenhirState67
  | MenhirState63
  | MenhirState60
  | MenhirState48
  | MenhirState46
  | MenhirState44
  | MenhirState42
  | MenhirState40
  | MenhirState38
  | MenhirState36
  | MenhirState34
  | MenhirState32
  | MenhirState30
  | MenhirState28
  | MenhirState26
  | MenhirState24
  | MenhirState22
  | MenhirState20
  | MenhirState18
  | MenhirState16
  | MenhirState14
  | MenhirState12
  | MenhirState8
  | MenhirState6
  | MenhirState5
  | MenhirState0

# 1 "minic_parser.mly"
  
  open Lexing
  open Minic_ast

# 119 "minic_parser.ml"

let rec _menhir_goto_list_instruction_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Minic_ast.seq) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState97 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (s : (Minic_ast.seq))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Minic_ast.instr) = 
# 119 "minic_parser.mly"
                                      ( Scope(s) )
# 140 "minic_parser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState105 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Minic_ast.instr))), _, (xs : (Minic_ast.seq))) = _menhir_stack in
        let _v : (Minic_ast.seq) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 156 "minic_parser.ml"
         in
        _menhir_goto_list_instruction_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState96 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ELSE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BEGIN ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | BEGIN ->
                        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState110
                    | BOOL ->
                        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState110
                    | IDENT _v ->
                        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v
                    | IF ->
                        _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState110
                    | INT ->
                        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState110
                    | PUTCHAR ->
                        _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState110
                    | RETURN ->
                        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState110
                    | VOID ->
                        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState110
                    | WHILE ->
                        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState110
                    | END ->
                        _menhir_reduce45 _menhir_env (Obj.magic _menhir_stack) MenhirState110
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState110)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | BEGIN | BOOL | END | IDENT _ | IF | INT | PUTCHAR | RETURN | VOID | WHILE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((((_menhir_stack, _menhir_s), _, (c : (Minic_ast.expr))), _), _, (s : (Minic_ast.seq))) = _menhir_stack in
                let _7 = () in
                let _5 = () in
                let _4 = () in
                let _2 = () in
                let _1 = () in
                let _v : (Minic_ast.instr) = 
# 103 "minic_parser.mly"
                                                                ( If(c, s, []) )
# 220 "minic_parser.ml"
                 in
                _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState110 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s), _, (c : (Minic_ast.expr))), _), _, (s1 : (Minic_ast.seq))), _, (s2 : (Minic_ast.seq))) = _menhir_stack in
            let _11 = () in
            let _9 = () in
            let _8 = () in
            let _7 = () in
            let _5 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Minic_ast.instr) = 
# 101 "minic_parser.mly"
                                                                                                       ( If(c,s1,s2) )
# 256 "minic_parser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (c : (Minic_ast.expr))), _, (s : (Minic_ast.seq))) = _menhir_stack in
            let _7 = () in
            let _5 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Minic_ast.instr) = 
# 112 "minic_parser.mly"
                                                                   ( While(c,s) )
# 283 "minic_parser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s, (t : (Minic_ast.typ))), (f : (
# 10 "minic_parser.mly"
       (string)
# 304 "minic_parser.ml"
            ))), _, (p : ((Minic_ast.typ * string) list))), _, (s : (Minic_ast.seq))) = _menhir_stack in
            let _8 = () in
            let _6 = () in
            let _5 = () in
            let _3 = () in
            let _v : (Minic_ast.fun_def) = 
# 83 "minic_parser.mly"
    ( { 
        name = f;
        code = s;
        params = p;
        return = t;
    } )
# 318 "minic_parser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (fd : (Minic_ast.fun_def)) = _v in
            let _v : (Minic_ast.global_scope_def) = 
# 49 "minic_parser.mly"
                       (Function(fd))
# 326 "minic_parser.ml"
             in
            _menhir_goto_global_scope_def _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_global_scope_def : _menhir_env -> 'ttv_tail -> _menhir_state -> (Minic_ast.global_scope_def) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState123
    | IDENT _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v
    | INT ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState123
    | VOID ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState123
    | EOF ->
        _menhir_reduce30 _menhir_env (Obj.magic _menhir_stack) MenhirState123
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState123

and _menhir_goto_instruction : _menhir_env -> 'ttv_tail -> _menhir_state -> (Minic_ast.instr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState71 | MenhirState77 | MenhirState96 | MenhirState110 | MenhirState105 | MenhirState97 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | BOOL ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | IDENT _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
        | IF ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | INT ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | PUTCHAR ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | RETURN ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | VOID ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | WHILE ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | END ->
            _menhir_reduce45 _menhir_env (Obj.magic _menhir_stack) MenhirState105
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState105)
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ELSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | BOOL ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | IDENT _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
            | IF ->
                _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | INT ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | PUTCHAR ->
                _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | RETURN ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | VOID ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | WHILE ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState114)
        | BEGIN | BOOL | END | IDENT _ | IF | INT | PUTCHAR | RETURN | VOID | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (c : (Minic_ast.expr))), _, (i : (Minic_ast.instr))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Minic_ast.instr) = 
# 107 "minic_parser.mly"
                                                 ( If(c, [i], []) )
# 434 "minic_parser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState114 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s), _, (c : (Minic_ast.expr))), _, (i1 : (Minic_ast.instr))), _, (i2 : (Minic_ast.instr))) = _menhir_stack in
        let _6 = () in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _v : (Minic_ast.instr) = 
# 105 "minic_parser.mly"
                                                                       ( If(c, [i1], [i2]) )
# 454 "minic_parser.ml"
         in
        _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce45 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Minic_ast.seq) = 
# 211 "<standard.mly>"
    ( [] )
# 465 "minic_parser.ml"
     in
    _menhir_goto_list_instruction_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run72 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BOOL_CST _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
        | CST _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
        | IDENT _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
        | LPAR ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run78 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState78 in
        let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BOOL_CST _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
        | CST _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
        | IDENT _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
        | LPAR ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState80)
    | BOOL_CST _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | CST _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | IDENT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | LPAR ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | SEMI ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState78 in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : (Minic_ast.instr) = 
# 98 "minic_parser.mly"
                  ( Return(Undef) )
# 543 "minic_parser.ml"
         in
        _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState78

and _menhir_run86 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BOOL_CST _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
        | CST _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
        | IDENT _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
        | LPAR ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run91 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BOOL_CST _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
        | CST _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
        | IDENT _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
        | LPAR ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState92)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run97 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | BOOL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | IDENT _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
    | IF ->
        _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | INT ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | PUTCHAR ->
        _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | RETURN ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | VOID ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | WHILE ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | END ->
        _menhir_reduce45 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState97

and _menhir_goto_call_list : _menhir_env -> 'ttv_tail -> _menhir_state -> (Minic_ast.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (e : (Minic_ast.expr))), _, (cl : (Minic_ast.expr list))) = _menhir_stack in
        let _2 = () in
        let _v : (Minic_ast.expr list) = 
# 125 "minic_parser.mly"
                                        ( e::cl )
# 654 "minic_parser.ml"
         in
        _menhir_goto_call_list _menhir_env _menhir_stack _menhir_s _v
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (f : (
# 10 "minic_parser.mly"
       (string)
# 669 "minic_parser.ml"
            ))), _, (p : (Minic_ast.expr list))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 129 "minic_parser.mly"
                                  (Call(f, p))
# 676 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run12 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | CST _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | IDENT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | LPAR ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12

and _menhir_run14 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | CST _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | IDENT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | LPAR ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14

and _menhir_run16 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | CST _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | IDENT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | LPAR ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16

and _menhir_run18 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | CST _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | IDENT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | LPAR ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18

and _menhir_run20 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | CST _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | IDENT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | LPAR ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState20

and _menhir_run22 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
    | CST _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
    | IDENT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
    | LPAR ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState22

and _menhir_run24 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
    | CST _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
    | IDENT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
    | LPAR ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState24

and _menhir_run26 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | CST _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | IDENT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | LPAR ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26

and _menhir_run28 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
    | CST _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
    | IDENT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
    | LPAR ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState28

and _menhir_run30 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | CST _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | IDENT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | LPAR ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30

and _menhir_run32 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | CST _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | IDENT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | LPAR ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32

and _menhir_run34 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | CST _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | IDENT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | LPAR ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34

and _menhir_run36 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | CST _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | IDENT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | LPAR ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36

and _menhir_run38 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | CST _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | IDENT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | LPAR ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38

and _menhir_run40 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | CST _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | IDENT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | LPAR ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40

and _menhir_run42 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | CST _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | IDENT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | LPAR ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42

and _menhir_run44 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | CST _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | IDENT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | LPAR ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState44

and _menhir_run46 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | CST _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | IDENT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | LPAR ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46

and _menhir_goto_variable_decl : _menhir_env -> 'ttv_tail -> _menhir_state -> (Minic_ast.typ * string * Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState123 | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (vd : (Minic_ast.typ * string * Minic_ast.expr)) = _v in
        let _v : (Minic_ast.global_scope_def) = 
# 48 "minic_parser.mly"
                       (Variable(vd))
# 1022 "minic_parser.ml"
         in
        _menhir_goto_global_scope_def _menhir_env _menhir_stack _menhir_s _v
    | MenhirState71 | MenhirState77 | MenhirState114 | MenhirState94 | MenhirState110 | MenhirState96 | MenhirState105 | MenhirState97 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (decl : (Minic_ast.typ * string * Minic_ast.expr)) = _v in
        let _v : (Minic_ast.instr) = 
# 116 "minic_parser.mly"
                           ( Set(decl) )
# 1032 "minic_parser.ml"
         in
        _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_parameter_list : _menhir_env -> 'ttv_tail -> _menhir_state -> ((Minic_ast.typ * string) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (d : (Minic_ast.typ * string * Minic_ast.expr))), _, (p : ((Minic_ast.typ * string) list))) = _menhir_stack in
        let _2 = () in
        let _v : ((Minic_ast.typ * string) list) = 
# 77 "minic_parser.mly"
                                                 ( let (t, i, _) = d in (t,i)::p )
# 1050 "minic_parser.ml"
         in
        _menhir_goto_parameter_list _menhir_env _menhir_stack _menhir_s _v
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BEGIN ->
                    _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState71
                | BOOL ->
                    _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState71
                | IDENT _v ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
                | IF ->
                    _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState71
                | INT ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState71
                | PUTCHAR ->
                    _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState71
                | RETURN ->
                    _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState71
                | VOID ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState71
                | WHILE ->
                    _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState71
                | END ->
                    _menhir_reduce45 _menhir_env (Obj.magic _menhir_stack) MenhirState71
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Minic_ast.expr list) = 
# 123 "minic_parser.mly"
    ( [] )
# 1112 "minic_parser.ml"
     in
    _menhir_goto_call_list _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState48 | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | SEPARATOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL_CST _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
            | CST _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
            | IDENT _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
            | LPAR ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState48
            | RPAR ->
                _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState48
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48)
        | SUB ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (e : (Minic_ast.expr))) = _menhir_stack in
            let _v : (Minic_ast.expr list) = 
# 124 "minic_parser.mly"
                 ( [e] )
# 1186 "minic_parser.ml"
             in
            _menhir_goto_call_list _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | END | RPAR | SEMI | SEPARATOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 145 "minic_parser.mly"
                                     (Xor(a,b))
# 1243 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | END | RPAR | SEMI | SEPARATOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 154 "minic_parser.mly"
                                     (Sub(a,b))
# 1300 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | END | RPAR | SEMI | SEPARATOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 143 "minic_parser.mly"
                                    (Or(a,b))
# 1357 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | END | RPAR | SEMI | SEPARATOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 147 "minic_parser.mly"
                                     (Neq(a,b))
# 1414 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | END | RPAR | SEMI | SEPARATOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 151 "minic_parser.mly"
                                     (Mul(a,b))
# 1471 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | END | RPAR | SEMI | SEPARATOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 153 "minic_parser.mly"
                                     (Mod(a,b))
# 1528 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState24 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | END | RPAR | SEMI | SEPARATOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 135 "minic_parser.mly"
                                    (Lt(a,b))
# 1585 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | END | RPAR | SEMI | SEPARATOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 137 "minic_parser.mly"
                                     (Leqt(a,b))
# 1642 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | END | RPAR | SEMI | SEPARATOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 136 "minic_parser.mly"
                                    (Gt(a,b))
# 1699 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | END | RPAR | SEMI | SEPARATOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 138 "minic_parser.mly"
                                     (Geqt(a,b))
# 1756 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | END | RPAR | SEMI | SEPARATOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 139 "minic_parser.mly"
                                    (Eq(a,b))
# 1813 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | END | RPAR | SEMI | SEPARATOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 152 "minic_parser.mly"
                                     (Div(a,b))
# 1870 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | END | RPAR | SEMI | SEPARATOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 146 "minic_parser.mly"
                                      (BXor(a,b))
# 1927 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | END | RPAR | SEMI | SEPARATOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 144 "minic_parser.mly"
                                     (BOr(a,b))
# 1984 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | END | RPAR | SEMI | SEPARATOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 148 "minic_parser.mly"
                                      (BNeq(a,b))
# 2041 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | END | RPAR | SEMI | SEPARATOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 142 "minic_parser.mly"
                                      (BAnd(a,b))
# 2098 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | END | RPAR | SEMI | SEPARATOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 141 "minic_parser.mly"
                                     (And(a,b))
# 2155 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | END | RPAR | SEMI | SEPARATOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 150 "minic_parser.mly"
                                     (Add(a,b))
# 2212 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Minic_ast.expr))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Minic_ast.expr) = 
# 133 "minic_parser.mly"
                           ( e )
# 2268 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | SUB ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (i : (
# 10 "minic_parser.mly"
       (string)
# 2325 "minic_parser.ml"
            ))), _, (v : (Minic_ast.expr))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (Minic_ast.typ * string * Minic_ast.expr) = 
# 70 "minic_parser.mly"
                                      ( (None, i, v) )
# 2332 "minic_parser.ml"
             in
            _menhir_goto_variable_decl _menhir_env _menhir_stack _menhir_s _v
        | SUB ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (t : (Minic_ast.typ))), (i : (
# 10 "minic_parser.mly"
       (string)
# 2389 "minic_parser.ml"
            ))), _, (v : (Minic_ast.expr))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _v : (Minic_ast.typ * string * Minic_ast.expr) = 
# 68 "minic_parser.mly"
                                              ( (t, i, v) )
# 2396 "minic_parser.ml"
             in
            _menhir_goto_variable_decl _menhir_env _menhir_stack _menhir_s _v
        | SUB ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BEGIN ->
                    _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState77
                | BOOL ->
                    _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState77
                | IDENT _v ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
                | IF ->
                    _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState77
                | INT ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState77
                | PUTCHAR ->
                    _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState77
                | RETURN ->
                    _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState77
                | VOID ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState77
                | WHILE ->
                    _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState77
                | END ->
                    _menhir_reduce45 _menhir_env (Obj.magic _menhir_stack) MenhirState77
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77)
            | SEMI ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _, (c : (Minic_ast.expr))) = _menhir_stack in
                let _5 = () in
                let _4 = () in
                let _2 = () in
                let _1 = () in
                let _v : (Minic_ast.instr) = 
# 114 "minic_parser.mly"
                                        ( While(c, []) )
# 2492 "minic_parser.ml"
                 in
                _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | SUB ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SEMI ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((_menhir_stack, _menhir_s), _), _, (e : (Minic_ast.expr))) = _menhir_stack in
                let _5 = () in
                let _4 = () in
                let _2 = () in
                let _1 = () in
                let _v : (Minic_ast.instr) = 
# 96 "minic_parser.mly"
                                         ( Return(e) )
# 2547 "minic_parser.ml"
                 in
                _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | EQ ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Minic_ast.expr))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Minic_ast.instr) = 
# 94 "minic_parser.mly"
                               ( Return(e) )
# 2631 "minic_parser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | SUB ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SEMI ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _, (e : (Minic_ast.expr))) = _menhir_stack in
                let _5 = () in
                let _4 = () in
                let _2 = () in
                let _1 = () in
                let _v : (Minic_ast.instr) = 
# 117 "minic_parser.mly"
                                          (Putchar(e))
# 2698 "minic_parser.ml"
                 in
                _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | SUB ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState92 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState94 in
                let _menhir_stack = (_menhir_stack, _menhir_s) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BEGIN ->
                    _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState96
                | BOOL ->
                    _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState96
                | IDENT _v ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
                | IF ->
                    _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState96
                | INT ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState96
                | PUTCHAR ->
                    _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState96
                | RETURN ->
                    _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState96
                | VOID ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState96
                | WHILE ->
                    _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState96
                | END ->
                    _menhir_reduce45 _menhir_env (Obj.magic _menhir_stack) MenhirState96
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState96)
            | BOOL ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | IDENT _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
            | IF ->
                _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | INT ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | PUTCHAR ->
                _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | RETURN ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | SEMI ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState94 in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _, (c : (Minic_ast.expr))) = _menhir_stack in
                let _5 = () in
                let _4 = () in
                let _2 = () in
                let _1 = () in
                let _v : (Minic_ast.instr) = 
# 109 "minic_parser.mly"
                                     ( If(c, [], []) )
# 2815 "minic_parser.ml"
                 in
                _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
            | VOID ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | WHILE ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState94)
        | SUB ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_reduce52 : _menhir_env -> ('ttv_tail * _menhir_state * (Minic_ast.typ)) * (
# 10 "minic_parser.mly"
       (string)
# 2847 "minic_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s, (t : (Minic_ast.typ))), (i : (
# 10 "minic_parser.mly"
       (string)
# 2853 "minic_parser.ml"
    ))) = _menhir_stack in
    let _v : (Minic_ast.typ * string * Minic_ast.expr) = 
# 63 "minic_parser.mly"
                      ( (t, i, Undef) )
# 2858 "minic_parser.ml"
     in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState67 | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SEPARATOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | INT ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | VOID ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | RPAR ->
                _menhir_reduce47 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67)
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (d : (Minic_ast.typ * string * Minic_ast.expr))) = _menhir_stack in
            let _v : ((Minic_ast.typ * string) list) = 
# 76 "minic_parser.mly"
                      ( let (t, i, _) = d in [(t,i)] )
# 2890 "minic_parser.ml"
             in
            _menhir_goto_parameter_list _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState123 | MenhirState0 | MenhirState71 | MenhirState77 | MenhirState114 | MenhirState94 | MenhirState110 | MenhirState96 | MenhirState105 | MenhirState97 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (d : (Minic_ast.typ * string * Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.typ * string * Minic_ast.expr) = 
# 69 "minic_parser.mly"
                             ( d )
# 2913 "minic_parser.ml"
             in
            _menhir_goto_variable_decl _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run60 : _menhir_env -> ('ttv_tail * _menhir_state * (Minic_ast.typ)) * (
# 10 "minic_parser.mly"
       (string)
# 2928 "minic_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
    | CST _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
    | IDENT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
    | LPAR ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60

and _menhir_reduce47 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((Minic_ast.typ * string) list) = 
# 75 "minic_parser.mly"
    ( [] )
# 2952 "minic_parser.ml"
     in
    _menhir_goto_parameter_list _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_program : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 28 "minic_parser.mly"
      (Minic_ast.prog)
# 2959 "minic_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 28 "minic_parser.mly"
      (Minic_ast.prog)
# 2967 "minic_parser.ml"
    )) = _v in
    Obj.magic _1

and _menhir_goto_global_scope_def_list : _menhir_env -> 'ttv_tail -> _menhir_state -> (Minic_ast.prog) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (p : (Minic_ast.prog))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 28 "minic_parser.mly"
      (Minic_ast.prog)
# 2988 "minic_parser.ml"
            ) = 
# 33 "minic_parser.mly"
                                (p)
# 2992 "minic_parser.ml"
             in
            _menhir_goto_program _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState123 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (d : (Minic_ast.global_scope_def))), _, (l : (Minic_ast.prog))) = _menhir_stack in
        let _v : (Minic_ast.prog) = 
# 44 "minic_parser.mly"
                                                 ( d::l )
# 3008 "minic_parser.ml"
         in
        _menhir_goto_global_scope_def_list _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | CST _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | IDENT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | LPAR ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState6

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 10 "minic_parser.mly"
       (string)
# 3036 "minic_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BOOL_CST _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
        | CST _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
        | IDENT _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
        | LPAR ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | RPAR ->
            _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8)
    | ADD | AND | BAND | BNEQ | BOR | BXOR | DIV | END | EQ | GET | GT | LET | LT | MOD | MUL | NEQ | OR | RPAR | SEMI | SEPARATOR | SUB | XOR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (i : (
# 10 "minic_parser.mly"
       (string)
# 3067 "minic_parser.ml"
        ))) = _menhir_stack in
        let _v : (Minic_ast.expr) = 
# 132 "minic_parser.mly"
            ( Get(i) )
# 3072 "minic_parser.ml"
         in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 8 "minic_parser.mly"
       (int)
# 3085 "minic_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (n : (
# 8 "minic_parser.mly"
       (int)
# 3093 "minic_parser.ml"
    )) = _v in
    let _v : (Minic_ast.expr) = 
# 130 "minic_parser.mly"
          ( Cst(n) )
# 3098 "minic_parser.ml"
     in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 9 "minic_parser.mly"
       (bool)
# 3105 "minic_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (b : (
# 9 "minic_parser.mly"
       (bool)
# 3113 "minic_parser.ml"
    )) = _v in
    let _v : (Minic_ast.expr) = 
# 131 "minic_parser.mly"
               ( BCst(b) )
# 3118 "minic_parser.ml"
     in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_typ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Minic_ast.typ) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState123 | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LPAR ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BOOL ->
                    _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState63
                | INT ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState63
                | VOID ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState63
                | RPAR ->
                    _menhir_reduce47 _menhir_env (Obj.magic _menhir_stack) MenhirState63
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63)
            | SET ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
            | SEMI ->
                _menhir_reduce52 _menhir_env (Obj.magic _menhir_stack)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState67 | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            _menhir_reduce52 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState71 | MenhirState77 | MenhirState114 | MenhirState94 | MenhirState110 | MenhirState96 | MenhirState105 | MenhirState97 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SET ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
            | SEMI ->
                _menhir_reduce52 _menhir_env (Obj.magic _menhir_stack)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState123 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState114 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState110 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState105 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState97 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState96 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState92 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState24 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState0 in
        let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos__1_ = _startpos in
        let _1 = () in
        let _v : (
# 28 "minic_parser.mly"
      (Minic_ast.prog)
# 3386 "minic_parser.ml"
        ) = let _startpos = _startpos__1_ in
        
# 35 "minic_parser.mly"
  ( 
      let pos = _startpos in
      let message = Printf.sprintf "Syntax error at line %d:%d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol) in 
      failwith message 
  )
# 3395 "minic_parser.ml"
         in
        _menhir_goto_program _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce30 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Minic_ast.prog) = 
# 43 "minic_parser.mly"
    ( [] )
# 3404 "minic_parser.ml"
     in
    _menhir_goto_global_scope_def_list _menhir_env _menhir_stack _menhir_s _v

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Minic_ast.typ) = 
# 55 "minic_parser.mly"
         ( Void )
# 3416 "minic_parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Minic_ast.typ) = 
# 53 "minic_parser.mly"
        ( Int )
# 3428 "minic_parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 10 "minic_parser.mly"
       (string)
# 3435 "minic_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SET ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BOOL_CST _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
        | CST _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
        | IDENT _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
        | LPAR ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState5
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState5)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run56 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Minic_ast.typ) = 
# 54 "minic_parser.mly"
         ( Bool )
# 3474 "minic_parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and program : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 28 "minic_parser.mly"
      (Minic_ast.prog)
# 3493 "minic_parser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env = let _tok = Obj.magic () in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    } in
    Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | IDENT _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | INT ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | VOID ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | EOF ->
        _menhir_reduce30 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

# 269 "<standard.mly>"
  

# 3525 "minic_parser.ml"
