
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
# 29 "minic_parser.ml"
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
# 41 "minic_parser.ml"
  )
    | COMMENT
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
  | MenhirState116
  | MenhirState110
  | MenhirState106
  | MenhirState101
  | MenhirState93
  | MenhirState92
  | MenhirState89
  | MenhirState87
  | MenhirState80
  | MenhirState78
  | MenhirState77
  | MenhirState73
  | MenhirState71
  | MenhirState67
  | MenhirState63
  | MenhirState60
  | MenhirState57
  | MenhirState51
  | MenhirState49
  | MenhirState47
  | MenhirState45
  | MenhirState43
  | MenhirState41
  | MenhirState39
  | MenhirState37
  | MenhirState35
  | MenhirState33
  | MenhirState31
  | MenhirState29
  | MenhirState27
  | MenhirState25
  | MenhirState23
  | MenhirState21
  | MenhirState19
  | MenhirState17
  | MenhirState10
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
    | MenhirState93 ->
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
# 115 "minic_parser.mly"
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
    | MenhirState101 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Minic_ast.instr))), _, (xs : (Minic_ast.seq))) = _menhir_stack in
        let _v : (Minic_ast.seq) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 156 "minic_parser.ml"
         in
        _menhir_goto_list_instruction_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState92 ->
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
                        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState106
                    | BOOL ->
                        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState106
                    | COMMENT ->
                        _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState106
                    | IDENT _v ->
                        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
                    | IF ->
                        _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState106
                    | INT ->
                        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState106
                    | RETURN ->
                        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState106
                    | VOID ->
                        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState106
                    | WHILE ->
                        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState106
                    | END ->
                        _menhir_reduce44 _menhir_env (Obj.magic _menhir_stack) MenhirState106
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState106)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | BEGIN | BOOL | COMMENT | END | IDENT _ | IF | INT | RETURN | VOID | WHILE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((((_menhir_stack, _menhir_s), _, (c : (Minic_ast.expr))), _), _, (s : (Minic_ast.seq))) = _menhir_stack in
                let _7 = () in
                let _5 = () in
                let _4 = () in
                let _2 = () in
                let _1 = () in
                let _v : (Minic_ast.instr) = 
# 100 "minic_parser.mly"
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
    | MenhirState106 ->
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
# 98 "minic_parser.mly"
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
# 109 "minic_parser.mly"
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
# 79 "minic_parser.mly"
  ( { 
      name = f; 
      code = s; 
      params = p; 
      return = t;
  } )
# 318 "minic_parser.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | IDENT _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
            | INT ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | VOID ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | EOF ->
                _menhir_reduce4 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState116)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_call_list : _menhir_env -> 'ttv_tail -> _menhir_state -> (Minic_ast.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (i : (
# 10 "minic_parser.mly"
       (string)
# 358 "minic_parser.ml"
        ))), _, (is : (Minic_ast.expr list))) = _menhir_stack in
        let _2 = () in
        let _v : (Minic_ast.expr list) = 
# 121 "minic_parser.mly"
                                   ( Get(i)::is )
# 364 "minic_parser.ml"
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
# 379 "minic_parser.ml"
            ))), _, (p : (Minic_ast.expr list))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 124 "minic_parser.mly"
                                  (Call(f, p))
# 386 "minic_parser.ml"
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

and _menhir_goto_instruction : _menhir_env -> 'ttv_tail -> _menhir_state -> (Minic_ast.instr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState71 | MenhirState77 | MenhirState92 | MenhirState106 | MenhirState101 | MenhirState93 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | BOOL ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | COMMENT ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | IDENT _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
        | IF ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | INT ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | RETURN ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | VOID ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | WHILE ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | END ->
            _menhir_reduce44 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState101)
    | MenhirState89 ->
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
                _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState110
            | BOOL ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState110
            | COMMENT ->
                _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState110
            | IDENT _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v
            | IF ->
                _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState110
            | INT ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState110
            | RETURN ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState110
            | VOID ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState110
            | WHILE ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState110
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState110)
        | BEGIN | BOOL | COMMENT | END | IDENT _ | IF | INT | RETURN | VOID | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (c : (Minic_ast.expr))), _, (i : (Minic_ast.instr))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Minic_ast.instr) = 
# 104 "minic_parser.mly"
                                                 ( If(c, [i], []) )
# 472 "minic_parser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState110 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s), _, (c : (Minic_ast.expr))), _, (i1 : (Minic_ast.instr))), _, (i2 : (Minic_ast.instr))) = _menhir_stack in
        let _6 = () in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _v : (Minic_ast.instr) = 
# 102 "minic_parser.mly"
                                                                       ( If(c, [i1], [i2]) )
# 492 "minic_parser.ml"
         in
        _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce44 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Minic_ast.seq) = 
# 211 "<standard.mly>"
    ( [] )
# 503 "minic_parser.ml"
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
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
        | CST _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
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
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
        | CST _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
        | IDENT _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
        | LPAR ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState80)
    | BOOL_CST _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
    | CST _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
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
# 95 "minic_parser.mly"
                  ( Return(Undef) )
# 581 "minic_parser.ml"
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
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
        | CST _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
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
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Minic_ast.instr) = 
# 88 "minic_parser.mly"
              (Skip)
# 627 "minic_parser.ml"
     in
    _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v

and _menhir_run93 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | BOOL ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | COMMENT ->
        _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | IDENT _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
    | IF ->
        _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | INT ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | RETURN ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | VOID ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | WHILE ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | END ->
        _menhir_reduce44 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState93

and _menhir_run17 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
    | CST _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
    | IDENT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
    | LPAR ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17

and _menhir_run19 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | CST _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | IDENT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | LPAR ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19

and _menhir_run21 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
    | CST _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
    | IDENT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
    | LPAR ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState21

and _menhir_run23 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | CST _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | IDENT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | LPAR ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState23

and _menhir_run25 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | CST _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | IDENT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | LPAR ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState25

and _menhir_run27 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | CST _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | IDENT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | LPAR ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState27

and _menhir_run29 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | CST _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | IDENT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | LPAR ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29

and _menhir_run31 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | CST _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | IDENT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | LPAR ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31

and _menhir_run33 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | CST _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | IDENT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | LPAR ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState33

and _menhir_run35 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | CST _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | IDENT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | LPAR ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35

and _menhir_run37 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | CST _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | IDENT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | LPAR ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState37

and _menhir_run39 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | CST _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | IDENT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | LPAR ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState39

and _menhir_run41 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | CST _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | IDENT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | LPAR ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41

and _menhir_run43 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | CST _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | IDENT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | LPAR ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState43

and _menhir_run45 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | CST _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | IDENT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | LPAR ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45

and _menhir_run47 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | CST _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | IDENT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | LPAR ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47

and _menhir_run49 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | CST _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | IDENT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | LPAR ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49

and _menhir_run51 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | CST _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | IDENT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | LPAR ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51

and _menhir_goto_variable_decl : _menhir_env -> 'ttv_tail -> _menhir_state -> (Minic_ast.typ * string * Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState116 | MenhirState57 | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BOOL ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | IDENT _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
        | INT ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | VOID ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | EOF ->
            _menhir_reduce4 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57)
    | MenhirState71 | MenhirState77 | MenhirState110 | MenhirState89 | MenhirState106 | MenhirState92 | MenhirState101 | MenhirState93 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (decl : (Minic_ast.typ * string * Minic_ast.expr))) = _menhir_stack in
        let _v : (Minic_ast.instr) = 
# 113 "minic_parser.mly"
                           ( Set(decl) )
# 1016 "minic_parser.ml"
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
# 67 "minic_parser.mly"
                                                 ( let (t, i, _) = d in (t,i)::p )
# 1034 "minic_parser.ml"
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
                    _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState71
                | BOOL ->
                    _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState71
                | COMMENT ->
                    _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState71
                | IDENT _v ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
                | IF ->
                    _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState71
                | INT ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState71
                | RETURN ->
                    _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState71
                | VOID ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState71
                | WHILE ->
                    _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState71
                | END ->
                    _menhir_reduce44 _menhir_env (Obj.magic _menhir_stack) MenhirState71
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
# 119 "minic_parser.mly"
    ( [] )
# 1096 "minic_parser.ml"
     in
    _menhir_goto_call_list _menhir_env _menhir_stack _menhir_s _v

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 10 "minic_parser.mly"
       (string)
# 1103 "minic_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SEPARATOR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
        | RPAR ->
            _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState10)
    | RPAR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (i : (
# 10 "minic_parser.mly"
       (string)
# 1128 "minic_parser.ml"
        ))) = _menhir_stack in
        let _v : (Minic_ast.expr list) = 
# 120 "minic_parser.mly"
            ( [Get(i)] )
# 1133 "minic_parser.ml"
         in
        _menhir_goto_call_list _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Minic_ast.expr))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Minic_ast.expr) = 
# 128 "minic_parser.mly"
                           ( e )
# 1194 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | SUB ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | END | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 140 "minic_parser.mly"
                                   (Xor(a,b))
# 1255 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | END | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 149 "minic_parser.mly"
                                   (Sub(a,b))
# 1312 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | END | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 138 "minic_parser.mly"
                                  (Or(a,b))
# 1369 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | END | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 142 "minic_parser.mly"
                                   (Neq(a,b))
# 1426 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | END | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 146 "minic_parser.mly"
                                   (Mul(a,b))
# 1483 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | END | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 148 "minic_parser.mly"
                                   (Mod(a,b))
# 1540 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | END | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 130 "minic_parser.mly"
                                  (Lt(a,b))
# 1597 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | END | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 132 "minic_parser.mly"
                                   (Leqt(a,b))
# 1654 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | END | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 131 "minic_parser.mly"
                                  (Gt(a,b))
# 1711 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | END | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 133 "minic_parser.mly"
                                   (Geqt(a,b))
# 1768 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | END | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 134 "minic_parser.mly"
                                  (Eq(a,b))
# 1825 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | END | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 147 "minic_parser.mly"
                                   (Div(a,b))
# 1882 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | END | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 141 "minic_parser.mly"
                                    (BXor(a,b))
# 1939 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | END | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 139 "minic_parser.mly"
                                   (BOr(a,b))
# 1996 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | END | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 143 "minic_parser.mly"
                                    (BNeq(a,b))
# 2053 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | END | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 137 "minic_parser.mly"
                                    (BAnd(a,b))
# 2110 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | END | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 136 "minic_parser.mly"
                                   (And(a,b))
# 2167 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | END | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 145 "minic_parser.mly"
                                   (Add(a,b))
# 2224 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
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
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (i : (
# 10 "minic_parser.mly"
       (string)
# 2277 "minic_parser.ml"
            ))), _, (v : (Minic_ast.expr))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (Minic_ast.typ * string * Minic_ast.expr) = 
# 73 "minic_parser.mly"
                                      ( (None, i, v) )
# 2284 "minic_parser.ml"
             in
            _menhir_goto_variable_decl _menhir_env _menhir_stack _menhir_s _v
        | SUB ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (t : (Minic_ast.typ))), (i : (
# 10 "minic_parser.mly"
       (string)
# 2341 "minic_parser.ml"
            ))), _, (v : (Minic_ast.expr))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _v : (Minic_ast.typ * string * Minic_ast.expr) = 
# 71 "minic_parser.mly"
                                              ( (t, i, v) )
# 2348 "minic_parser.ml"
             in
            _menhir_goto_variable_decl _menhir_env _menhir_stack _menhir_s _v
        | SUB ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
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
                    _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState77
                | BOOL ->
                    _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState77
                | COMMENT ->
                    _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState77
                | IDENT _v ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
                | IF ->
                    _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState77
                | INT ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState77
                | RETURN ->
                    _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState77
                | VOID ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState77
                | WHILE ->
                    _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState77
                | END ->
                    _menhir_reduce44 _menhir_env (Obj.magic _menhir_stack) MenhirState77
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
# 111 "minic_parser.mly"
                                        ( While(c, []) )
# 2444 "minic_parser.ml"
                 in
                _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | SUB ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
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
# 93 "minic_parser.mly"
                                         ( Return(e) )
# 2499 "minic_parser.ml"
                 in
                _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | EQ ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Minic_ast.expr))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Minic_ast.instr) = 
# 91 "minic_parser.mly"
                               ( Return(e) )
# 2583 "minic_parser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | SUB ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState89 in
                let _menhir_stack = (_menhir_stack, _menhir_s) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BEGIN ->
                    _menhir_run93 _menhir_env (Obj.magic _menhir_stack) MenhirState92
                | BOOL ->
                    _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState92
                | COMMENT ->
                    _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState92
                | IDENT _v ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
                | IF ->
                    _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState92
                | INT ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState92
                | RETURN ->
                    _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState92
                | VOID ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState92
                | WHILE ->
                    _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState92
                | END ->
                    _menhir_reduce44 _menhir_env (Obj.magic _menhir_stack) MenhirState92
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState92)
            | BOOL ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState89
            | COMMENT ->
                _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState89
            | IDENT _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
            | IF ->
                _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState89
            | INT ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState89
            | RETURN ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) MenhirState89
            | SEMI ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState89 in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _, (c : (Minic_ast.expr))) = _menhir_stack in
                let _5 = () in
                let _4 = () in
                let _2 = () in
                let _1 = () in
                let _v : (Minic_ast.instr) = 
# 106 "minic_parser.mly"
                                     ( If(c, [], []) )
# 2694 "minic_parser.ml"
                 in
                _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
            | VOID ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState89
            | WHILE ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState89
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState89)
        | SUB ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
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

and _menhir_reduce51 : _menhir_env -> ('ttv_tail * _menhir_state * (Minic_ast.typ)) * (
# 10 "minic_parser.mly"
       (string)
# 2726 "minic_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s, (t : (Minic_ast.typ))), (i : (
# 10 "minic_parser.mly"
       (string)
# 2732 "minic_parser.ml"
    ))) = _menhir_stack in
    let _v : (Minic_ast.typ * string * Minic_ast.expr) = 
# 62 "minic_parser.mly"
                      ( (t, i, Undef) )
# 2737 "minic_parser.ml"
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
                _menhir_reduce46 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67)
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (d : (Minic_ast.typ * string * Minic_ast.expr))) = _menhir_stack in
            let _v : ((Minic_ast.typ * string) list) = 
# 66 "minic_parser.mly"
                      ( let (t, i, _) = d in [(t,i)] )
# 2769 "minic_parser.ml"
             in
            _menhir_goto_parameter_list _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState0 | MenhirState116 | MenhirState57 | MenhirState71 | MenhirState77 | MenhirState110 | MenhirState89 | MenhirState106 | MenhirState92 | MenhirState101 | MenhirState93 ->
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
# 72 "minic_parser.mly"
                             ( d )
# 2792 "minic_parser.ml"
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
# 2807 "minic_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
    | CST _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
    | IDENT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
    | LPAR ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60

and _menhir_reduce46 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((Minic_ast.typ * string) list) = 
# 65 "minic_parser.mly"
    ( [] )
# 2831 "minic_parser.ml"
     in
    _menhir_goto_parameter_list _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_program : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 28 "minic_parser.mly"
      (Minic_ast.prog)
# 2838 "minic_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 28 "minic_parser.mly"
      (Minic_ast.prog)
# 2846 "minic_parser.ml"
    )) = _v in
    Obj.magic _1

and _menhir_goto_declaration_list : _menhir_env -> 'ttv_tail -> _menhir_state -> ((Minic_ast.typ * string * Minic_ast.expr) list * Minic_ast.fun_def list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState116 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (fd : (Minic_ast.fun_def))), _, (dl : ((Minic_ast.typ * string * Minic_ast.expr) list * Minic_ast.fun_def list))) = _menhir_stack in
        let _v : ((Minic_ast.typ * string * Minic_ast.expr) list * Minic_ast.fun_def list) = 
# 51 "minic_parser.mly"
                                             ( let vl, fl = dl in vl, fd :: fl )
# 2861 "minic_parser.ml"
         in
        _menhir_goto_declaration_list _menhir_env _menhir_stack _menhir_s _v
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (vd : (Minic_ast.typ * string * Minic_ast.expr))), _, (dl : ((Minic_ast.typ * string * Minic_ast.expr) list * Minic_ast.fun_def list))) = _menhir_stack in
        let _v : ((Minic_ast.typ * string * Minic_ast.expr) list * Minic_ast.fun_def list) = 
# 50 "minic_parser.mly"
                                             ( let vl, fl = dl in vd :: vl, fl )
# 2871 "minic_parser.ml"
         in
        _menhir_goto_declaration_list _menhir_env _menhir_stack _menhir_s _v
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (dl : ((Minic_ast.typ * string * Minic_ast.expr) list * Minic_ast.fun_def list))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 28 "minic_parser.mly"
      (Minic_ast.prog)
# 2887 "minic_parser.ml"
            ) = 
# 34 "minic_parser.mly"
  ( let var_list, fun_list = dl in 
    { 
      globals = var_list; 
      functions = fun_list; 
    } 
  )
# 2896 "minic_parser.ml"
             in
            _menhir_goto_program _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | CST _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
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
# 2930 "minic_parser.ml"
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
        | IDENT _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
        | RPAR ->
            _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8)
    | ADD | AND | BAND | BNEQ | BOR | BXOR | DIV | END | EQ | GET | GT | LET | LT | MOD | MUL | NEQ | OR | RPAR | SEMI | SUB | XOR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (i : (
# 10 "minic_parser.mly"
       (string)
# 2955 "minic_parser.ml"
        ))) = _menhir_stack in
        let _v : (Minic_ast.expr) = 
# 127 "minic_parser.mly"
            ( Get(i) )
# 2960 "minic_parser.ml"
         in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run14 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 8 "minic_parser.mly"
       (int)
# 2973 "minic_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (n : (
# 8 "minic_parser.mly"
       (int)
# 2981 "minic_parser.ml"
    )) = _v in
    let _v : (Minic_ast.expr) = 
# 125 "minic_parser.mly"
          ( Cst(n) )
# 2986 "minic_parser.ml"
     in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run15 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 9 "minic_parser.mly"
       (bool)
# 2993 "minic_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (b : (
# 9 "minic_parser.mly"
       (bool)
# 3001 "minic_parser.ml"
    )) = _v in
    let _v : (Minic_ast.expr) = 
# 126 "minic_parser.mly"
               ( BCst(b) )
# 3006 "minic_parser.ml"
     in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_typ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Minic_ast.typ) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 | MenhirState116 | MenhirState57 ->
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
                    _menhir_reduce46 _menhir_env (Obj.magic _menhir_stack) MenhirState63
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63)
            | SET ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
            | SEMI ->
                _menhir_reduce51 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_reduce51 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState71 | MenhirState77 | MenhirState110 | MenhirState89 | MenhirState106 | MenhirState92 | MenhirState101 | MenhirState93 ->
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
                _menhir_reduce51 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState116 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState110 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState106 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState101 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState93 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState92 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
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
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState10 ->
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
# 3274 "minic_parser.ml"
        ) = let _startpos = _startpos__1_ in
        
# 41 "minic_parser.mly"
  ( 
    let pos = _startpos in
    let message = Printf.sprintf "Syntax error at line %d:%d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol) in 
    failwith message 
  )
# 3283 "minic_parser.ml"
         in
        _menhir_goto_program _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((Minic_ast.typ * string * Minic_ast.expr) list * Minic_ast.fun_def list) = 
# 49 "minic_parser.mly"
    ( [], [] )
# 3292 "minic_parser.ml"
     in
    _menhir_goto_declaration_list _menhir_env _menhir_stack _menhir_s _v

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Minic_ast.typ) = 
# 58 "minic_parser.mly"
         ( Void )
# 3304 "minic_parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Minic_ast.typ) = 
# 56 "minic_parser.mly"
        ( Int )
# 3316 "minic_parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 10 "minic_parser.mly"
       (string)
# 3323 "minic_parser.ml"
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
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
        | CST _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
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
# 57 "minic_parser.mly"
         ( Bool )
# 3362 "minic_parser.ml"
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
# 3381 "minic_parser.ml"
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
        _menhir_reduce4 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

# 269 "<standard.mly>"
  

# 3413 "minic_parser.ml"
