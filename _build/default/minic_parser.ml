
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | XOR
    | WHILE
    | VOID
    | SWITCH
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
# 31 "minic_parser.ml"
  )
    | GT
    | GET
    | FOR
    | EQ
    | EOF
    | END
    | ELSE
    | DOTS2
    | DO
    | DIV
    | DEFAULT
    | CST of (
# 8 "minic_parser.mly"
       (int)
# 47 "minic_parser.ml"
  )
    | CONTINUE
    | CASE
    | BXOR
    | BREAK
    | BOR
    | BOOL_CST of (
# 9 "minic_parser.mly"
       (bool)
# 57 "minic_parser.ml"
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
  | MenhirState191
  | MenhirState181
  | MenhirState176
  | MenhirState174
  | MenhirState170
  | MenhirState166
  | MenhirState163
  | MenhirState160
  | MenhirState153
  | MenhirState147
  | MenhirState137
  | MenhirState132
  | MenhirState130
  | MenhirState127
  | MenhirState126
  | MenhirState121
  | MenhirState119
  | MenhirState116
  | MenhirState114
  | MenhirState109
  | MenhirState102
  | MenhirState100
  | MenhirState99
  | MenhirState97
  | MenhirState95
  | MenhirState94
  | MenhirState91
  | MenhirState89
  | MenhirState85
  | MenhirState83
  | MenhirState79
  | MenhirState75
  | MenhirState73
  | MenhirState57
  | MenhirState54
  | MenhirState52
  | MenhirState50
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
  | MenhirState13
  | MenhirState11
  | MenhirState10
  | MenhirState0

# 1 "minic_parser.mly"
  
  open Lexing
  open Minic_ast

# 143 "minic_parser.ml"

let rec _menhir_goto_block_case_list : _menhir_env -> 'ttv_tail -> _menhir_state -> ((Minic_ast.expr list * Minic_ast.seq) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState176 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (ecl : (Minic_ast.expr list))), _, (s : (Minic_ast.seq))), _, (cl : ((Minic_ast.expr list * Minic_ast.seq) list))) = _menhir_stack in
        let _v : ((Minic_ast.expr list * Minic_ast.seq) list) = 
# 114 "minic_parser.mly"
                                                              ((ecl,s)::cl)
# 156 "minic_parser.ml"
         in
        _menhir_goto_block_case_list _menhir_env _menhir_stack _menhir_s _v
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DEFAULT ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | DOTS2 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | ADD ->
                    _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState181
                | BEGIN ->
                    _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState181
                | BOOL ->
                    _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState181
                | BREAK ->
                    _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState181
                | CONTINUE ->
                    _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState181
                | DO ->
                    _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState181
                | FOR ->
                    _menhir_run118 _menhir_env (Obj.magic _menhir_stack) MenhirState181
                | IDENT _v ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState181 _v
                | IF ->
                    _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState181
                | INT ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState181
                | PUTCHAR ->
                    _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState181
                | RETURN ->
                    _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState181
                | SUB ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState181
                | SWITCH ->
                    _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState181
                | VOID ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState181
                | WHILE ->
                    _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState181
                | END ->
                    _menhir_reduce56 _menhir_env (Obj.magic _menhir_stack) MenhirState181
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState181)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (e : (Minic_ast.expr))), _, (cl : ((Minic_ast.expr list * Minic_ast.seq) list))) = _menhir_stack in
            let _7 = () in
            let _5 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Minic_ast.instr) = 
# 145 "minic_parser.mly"
                                                                 ( Switch(e,cl, []) )
# 231 "minic_parser.ml"
             in
            _menhir_goto_conditional _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_instruction_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Minic_ast.seq) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState137 ->
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
# 163 "minic_parser.mly"
                                    ( Scope(s) )
# 262 "minic_parser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState147 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Minic_ast.instr))), _, (xs : (Minic_ast.seq))) = _menhir_stack in
        let _v : (Minic_ast.seq) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 278 "minic_parser.ml"
         in
        _menhir_goto_list_instruction_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState132 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | WHILE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | LPAR ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | BOOL_CST _v ->
                        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _v
                    | CST _v ->
                        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _v
                    | IDENT _v ->
                        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _v
                    | LPAR ->
                        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState153
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState153)
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
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState130 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s), _, (init_instr : (Minic_ast.seq))), _), _, (incr_instr : (Minic_ast.seq))), _, (s : (Minic_ast.seq))) = _menhir_stack in
            let _10 = () in
            let _8 = () in
            let _7 = () in
            let _5 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Minic_ast.instr) = 
# 128 "minic_parser.mly"
                                                                                                    ( For(init_instr, BCst(true), incr_instr, s) )
# 351 "minic_parser.ml"
             in
            _menhir_goto_loop _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState163 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s), _, (init_instr : (Minic_ast.seq))), _, (cond : (Minic_ast.expr))), _, (incr_instr : (Minic_ast.seq))), _, (s : (Minic_ast.seq))) = _menhir_stack in
            let _11 = () in
            let _9 = () in
            let _8 = () in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Minic_ast.instr) = 
# 127 "minic_parser.mly"
                                                                                                                      ( For(init_instr, cond, incr_instr, s) )
# 380 "minic_parser.ml"
             in
            _menhir_goto_loop _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState166 ->
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
                    | ADD ->
                        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState170
                    | BEGIN ->
                        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState170
                    | BOOL ->
                        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState170
                    | BREAK ->
                        _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState170
                    | CONTINUE ->
                        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState170
                    | DO ->
                        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState170
                    | FOR ->
                        _menhir_run118 _menhir_env (Obj.magic _menhir_stack) MenhirState170
                    | IDENT _v ->
                        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _v
                    | IF ->
                        _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState170
                    | INT ->
                        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState170
                    | PUTCHAR ->
                        _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState170
                    | RETURN ->
                        _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState170
                    | SUB ->
                        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState170
                    | SWITCH ->
                        _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState170
                    | VOID ->
                        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState170
                    | WHILE ->
                        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState170
                    | END ->
                        _menhir_reduce56 _menhir_env (Obj.magic _menhir_stack) MenhirState170
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState170)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | ADD | BEGIN | BOOL | BREAK | CASE | CONTINUE | DEFAULT | DO | END | FOR | IDENT _ | IF | INT | PUTCHAR | RETURN | SUB | SWITCH | VOID | WHILE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((((_menhir_stack, _menhir_s), _, (c : (Minic_ast.expr))), _), _, (s : (Minic_ast.seq))) = _menhir_stack in
                let _7 = () in
                let _5 = () in
                let _4 = () in
                let _2 = () in
                let _1 = () in
                let _v : (Minic_ast.instr) = 
# 136 "minic_parser.mly"
                                                                ( If(c, s, []) )
# 464 "minic_parser.ml"
                 in
                _menhir_goto_conditional _menhir_env _menhir_stack _menhir_s _v
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
    | MenhirState170 ->
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
# 134 "minic_parser.mly"
                                                                                                       ( If(c,s1,s2) )
# 500 "minic_parser.ml"
             in
            _menhir_goto_conditional _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CASE ->
            _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | DEFAULT | END ->
            _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState176
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState176)
    | MenhirState181 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (e : (Minic_ast.expr))), _, (cl : ((Minic_ast.expr list * Minic_ast.seq) list))), _, (def : (Minic_ast.seq))) = _menhir_stack in
            let _10 = () in
            let _8 = () in
            let _7 = () in
            let _5 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Minic_ast.instr) = 
# 144 "minic_parser.mly"
                                                                                                     ( Switch(e, cl, def) )
# 542 "minic_parser.ml"
             in
            _menhir_goto_conditional _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState89 ->
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
# 120 "minic_parser.mly"
                                                                   ( While(c,s) )
# 569 "minic_parser.ml"
             in
            _menhir_goto_loop _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState83 ->
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
# 590 "minic_parser.ml"
            ))), _, (p : ((Minic_ast.typ * string) list))), _, (s : (Minic_ast.seq))) = _menhir_stack in
            let _8 = () in
            let _6 = () in
            let _5 = () in
            let _3 = () in
            let _v : (Minic_ast.fun_def) = 
# 93 "minic_parser.mly"
    ( { 
        name = f;
        code = s;
        params = p;
        return = t;
    } )
# 604 "minic_parser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (fd : (Minic_ast.fun_def)) = _v in
            let _v : (Minic_ast.global_scope_def) = 
# 54 "minic_parser.mly"
                       (Function(fd))
# 612 "minic_parser.ml"
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

and _menhir_goto_conditional : _menhir_env -> 'ttv_tail -> _menhir_state -> (Minic_ast.instr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (c : (Minic_ast.instr)) = _v in
    let _v : (Minic_ast.instr) = 
# 160 "minic_parser.mly"
                  (c)
# 632 "minic_parser.ml"
     in
    _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_return : _menhir_env -> 'ttv_tail -> _menhir_state -> (Minic_ast.instr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (r : (Minic_ast.instr)) = _v in
    let _v : (Minic_ast.instr) = 
# 161 "minic_parser.mly"
             (r)
# 644 "minic_parser.ml"
     in
    _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_expr_case_list : _menhir_env -> 'ttv_tail -> _menhir_state -> (Minic_ast.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState97 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), _, (e : (Minic_ast.expr))), _, (ecl : (Minic_ast.expr list))) = _menhir_stack in
        let _3 = () in
        let _1 = () in
        let _v : (Minic_ast.expr list) = 
# 109 "minic_parser.mly"
                                               ( e::ecl )
# 661 "minic_parser.ml"
         in
        _menhir_goto_expr_case_list _menhir_env _menhir_stack _menhir_s _v
    | MenhirState176 | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | BEGIN ->
            _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | BOOL ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | BREAK ->
            _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | CONTINUE ->
            _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | DO ->
            _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | FOR ->
            _menhir_run118 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | IDENT _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
        | IF ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | INT ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | PUTCHAR ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | RETURN ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | SUB ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | SWITCH ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | VOID ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | WHILE ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | CASE | DEFAULT | END ->
            _menhir_reduce56 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState99)
    | _ ->
        _menhir_fail ()

and _menhir_reduce1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((Minic_ast.expr list * Minic_ast.seq) list) = 
# 113 "minic_parser.mly"
    ( [] )
# 715 "minic_parser.ml"
     in
    _menhir_goto_block_case_list _menhir_env _menhir_stack _menhir_s _v

and _menhir_run95 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
    | CST _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
    | IDENT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
    | LPAR ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState95

and _menhir_goto_loop : _menhir_env -> 'ttv_tail -> _menhir_state -> (Minic_ast.instr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (l : (Minic_ast.instr)) = _v in
    let _v : (Minic_ast.instr) = 
# 159 "minic_parser.mly"
           (l)
# 746 "minic_parser.ml"
     in
    _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_int_op : _menhir_env -> 'ttv_tail -> _menhir_state -> (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (iop : (Minic_ast.expr)) = _v in
    let _v : (Minic_ast.expr) = 
# 207 "minic_parser.mly"
               (iop)
# 758 "minic_parser.ml"
     in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_bool_op : _menhir_env -> 'ttv_tail -> _menhir_state -> (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (bop : (Minic_ast.expr)) = _v in
    let _v : (Minic_ast.expr) = 
# 206 "minic_parser.mly"
                (bop)
# 770 "minic_parser.ml"
     in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_funcall_args : _menhir_env -> 'ttv_tail -> _menhir_state -> (Minic_ast.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState13 ->
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
# 790 "minic_parser.ml"
            ))), _, (p : (Minic_ast.expr list))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 201 "minic_parser.mly"
                                     (Call(f, p))
# 797 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (e : (Minic_ast.expr))), _, (cl : (Minic_ast.expr list))) = _menhir_stack in
        let _2 = () in
        let _v : (Minic_ast.expr list) = 
# 172 "minic_parser.mly"
                                           ( e::cl )
# 814 "minic_parser.ml"
         in
        _menhir_goto_funcall_args _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run20 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | CST _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | IDENT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | LPAR ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState20
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
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
    | CST _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
    | IDENT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
    | LPAR ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState22
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
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
    | CST _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
    | IDENT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
    | LPAR ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState24
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
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | CST _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | IDENT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | LPAR ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState26
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
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
    | CST _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
    | IDENT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
    | LPAR ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState28
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
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | CST _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | IDENT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | LPAR ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState30
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
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | CST _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | IDENT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | LPAR ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState32
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
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | CST _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | IDENT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | LPAR ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState34
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
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | CST _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | IDENT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | LPAR ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState36
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
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | CST _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | IDENT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | LPAR ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState38
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
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | CST _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | IDENT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | LPAR ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState40
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
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | CST _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | IDENT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | LPAR ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState42
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
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | CST _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | IDENT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | LPAR ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState44
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
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | CST _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | IDENT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | LPAR ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46

and _menhir_run48 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | CST _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | IDENT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | LPAR ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48

and _menhir_run50 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | CST _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | IDENT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | LPAR ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50

and _menhir_run52 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | CST _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | IDENT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | LPAR ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52

and _menhir_run54 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | CST _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | IDENT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | LPAR ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54

and _menhir_goto_parameter_list : _menhir_env -> 'ttv_tail -> _menhir_state -> ((Minic_ast.typ * string) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (d : (Minic_ast.typ * string * Minic_ast.expr))), _, (p : ((Minic_ast.typ * string) list))) = _menhir_stack in
        let _2 = () in
        let _v : ((Minic_ast.typ * string) list) = 
# 88 "minic_parser.mly"
                                                 ( let (t, i, _) = d in (t,i)::p )
# 1156 "minic_parser.ml"
         in
        _menhir_goto_parameter_list _menhir_env _menhir_stack _menhir_s _v
    | MenhirState75 ->
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
                | ADD ->
                    _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState83
                | BEGIN ->
                    _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState83
                | BOOL ->
                    _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState83
                | BREAK ->
                    _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState83
                | CONTINUE ->
                    _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState83
                | DO ->
                    _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState83
                | FOR ->
                    _menhir_run118 _menhir_env (Obj.magic _menhir_stack) MenhirState83
                | IDENT _v ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
                | IF ->
                    _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState83
                | INT ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState83
                | PUTCHAR ->
                    _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState83
                | RETURN ->
                    _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState83
                | SUB ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState83
                | SWITCH ->
                    _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState83
                | VOID ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState83
                | WHILE ->
                    _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState83
                | END ->
                    _menhir_reduce56 _menhir_env (Obj.magic _menhir_stack) MenhirState83
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83)
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

and _menhir_reduce56 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Minic_ast.seq) = 
# 211 "<standard.mly>"
    ( [] )
# 1232 "minic_parser.ml"
     in
    _menhir_goto_list_instruction_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run84 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
        | CST _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
        | IDENT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
        | LPAR ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState85)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run90 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
        | CST _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
        | IDENT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
        | LPAR ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState91)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run100 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState100 in
        let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BOOL_CST _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
        | CST _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
        | IDENT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
        | LPAR ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState102)
    | BOOL_CST _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
    | CST _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
    | IDENT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
    | LPAR ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | SEMI ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState100 in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : (Minic_ast.instr) = 
# 155 "minic_parser.mly"
                  ( Return(Undef) )
# 1340 "minic_parser.ml"
         in
        _menhir_goto_return _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState100

and _menhir_run108 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
        | CST _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
        | IDENT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
        | LPAR ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState109
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState109)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run113 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
        | CST _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
        | IDENT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
        | LPAR ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState114)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run118 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
        | ADD ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | BOOL ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | IDENT _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _v
        | INT ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | SUB ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | VOID ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | SEMI ->
            _menhir_reduce32 _menhir_env (Obj.magic _menhir_stack) MenhirState119
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState119)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run131 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | BEGIN ->
            _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | BOOL ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | BREAK ->
            _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | CONTINUE ->
            _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | DO ->
            _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | FOR ->
            _menhir_run118 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | IDENT _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
        | IF ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | INT ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | PUTCHAR ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | RETURN ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | SUB ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | SWITCH ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | VOID ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | WHILE ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | END ->
            _menhir_reduce56 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState132)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run133 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SEMI ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : (Minic_ast.instr) = 
# 165 "minic_parser.mly"
                  ( Continue )
# 1516 "minic_parser.ml"
         in
        _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run135 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SEMI ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : (Minic_ast.instr) = 
# 166 "minic_parser.mly"
               ( Break )
# 1542 "minic_parser.ml"
         in
        _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run137 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADD ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState137
    | BEGIN ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState137
    | BOOL ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState137
    | BREAK ->
        _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState137
    | CONTINUE ->
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState137
    | DO ->
        _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState137
    | FOR ->
        _menhir_run118 _menhir_env (Obj.magic _menhir_stack) MenhirState137
    | IDENT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _v
    | IF ->
        _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState137
    | INT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState137
    | PUTCHAR ->
        _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState137
    | RETURN ->
        _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState137
    | SUB ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState137
    | SWITCH ->
        _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState137
    | VOID ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState137
    | WHILE ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState137
    | END ->
        _menhir_reduce56 _menhir_env (Obj.magic _menhir_stack) MenhirState137
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState137

and _menhir_goto_instruction : _menhir_env -> 'ttv_tail -> _menhir_state -> (Minic_ast.instr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState83 | MenhirState89 | MenhirState181 | MenhirState99 | MenhirState166 | MenhirState170 | MenhirState163 | MenhirState130 | MenhirState132 | MenhirState147 | MenhirState137 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | BEGIN ->
            _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | BOOL ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | BREAK ->
            _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | CONTINUE ->
            _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | DO ->
            _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | FOR ->
            _menhir_run118 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | IDENT _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
        | IF ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | INT ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | PUTCHAR ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | RETURN ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | SUB ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | SWITCH ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | VOID ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | WHILE ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | CASE | DEFAULT | END ->
            _menhir_reduce56 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState147)
    | MenhirState116 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ELSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ADD ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState174
            | BEGIN ->
                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState174
            | BOOL ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState174
            | BREAK ->
                _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState174
            | CONTINUE ->
                _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState174
            | DO ->
                _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState174
            | FOR ->
                _menhir_run118 _menhir_env (Obj.magic _menhir_stack) MenhirState174
            | IDENT _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _v
            | IF ->
                _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState174
            | INT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState174
            | PUTCHAR ->
                _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState174
            | RETURN ->
                _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState174
            | SUB ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState174
            | SWITCH ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState174
            | VOID ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState174
            | WHILE ->
                _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState174
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState174)
        | ADD | BEGIN | BOOL | BREAK | CASE | CONTINUE | DEFAULT | DO | END | FOR | IDENT _ | IF | INT | PUTCHAR | RETURN | SUB | SWITCH | VOID | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (c : (Minic_ast.expr))), _, (i : (Minic_ast.instr))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Minic_ast.instr) = 
# 140 "minic_parser.mly"
                                                 ( If(c, [i], []) )
# 1699 "minic_parser.ml"
             in
            _menhir_goto_conditional _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState174 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s), _, (c : (Minic_ast.expr))), _, (i1 : (Minic_ast.instr))), _, (i2 : (Minic_ast.instr))) = _menhir_stack in
        let _6 = () in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _v : (Minic_ast.instr) = 
# 138 "minic_parser.mly"
                                                                       ( If(c, [i1], [i2]) )
# 1719 "minic_parser.ml"
         in
        _menhir_goto_conditional _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_global_scope_def : _menhir_env -> 'ttv_tail -> _menhir_state -> (Minic_ast.global_scope_def) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADD ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState191
    | BOOL ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState191
    | IDENT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState191 _v
    | INT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState191
    | SUB ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState191
    | VOID ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState191
    | EOF ->
        _menhir_reduce41 _menhir_env (Obj.magic _menhir_stack) MenhirState191
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState191

and _menhir_reduce35 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Minic_ast.expr list) = 
# 170 "minic_parser.mly"
    ( [] )
# 1756 "minic_parser.ml"
     in
    _menhir_goto_funcall_args _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState57 | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | SEPARATOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL_CST _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
            | CST _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
            | IDENT _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
            | LPAR ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | RPAR ->
                _menhir_reduce35 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57)
        | SUB ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (e : (Minic_ast.expr))) = _menhir_stack in
            let _v : (Minic_ast.expr list) = 
# 171 "minic_parser.mly"
                 ( [e] )
# 1830 "minic_parser.ml"
             in
            _menhir_goto_funcall_args _menhir_env _menhir_stack _menhir_s _v
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
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | DOTS2 | END | RPAR | SEMI | SEPARATOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 186 "minic_parser.mly"
                                   (Xor(a,b))
# 1887 "minic_parser.ml"
             in
            _menhir_goto_bool_op _menhir_env _menhir_stack _menhir_s _v
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
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | DOTS2 | END | RPAR | SEMI | SEPARATOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 197 "minic_parser.mly"
                                   (Sub(a,b))
# 1944 "minic_parser.ml"
             in
            _menhir_goto_int_op _menhir_env _menhir_stack _menhir_s _v
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
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | DOTS2 | END | RPAR | SEMI | SEPARATOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 184 "minic_parser.mly"
                                  (Or(a,b))
# 2001 "minic_parser.ml"
             in
            _menhir_goto_bool_op _menhir_env _menhir_stack _menhir_s _v
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
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | DOTS2 | END | RPAR | SEMI | SEPARATOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 188 "minic_parser.mly"
                                   (Neq(a,b))
# 2058 "minic_parser.ml"
             in
            _menhir_goto_bool_op _menhir_env _menhir_stack _menhir_s _v
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
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | DOTS2 | END | RPAR | SEMI | SEPARATOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 194 "minic_parser.mly"
                                   (Mul(a,b))
# 2115 "minic_parser.ml"
             in
            _menhir_goto_int_op _menhir_env _menhir_stack _menhir_s _v
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
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | DOTS2 | END | RPAR | SEMI | SEPARATOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 196 "minic_parser.mly"
                                   (Mod(a,b))
# 2172 "minic_parser.ml"
             in
            _menhir_goto_int_op _menhir_env _menhir_stack _menhir_s _v
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
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | DOTS2 | END | RPAR | SEMI | SEPARATOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 176 "minic_parser.mly"
                                  (Lt(a,b))
# 2229 "minic_parser.ml"
             in
            _menhir_goto_bool_op _menhir_env _menhir_stack _menhir_s _v
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
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | DOTS2 | END | RPAR | SEMI | SEPARATOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 178 "minic_parser.mly"
                                   (Leqt(a,b))
# 2286 "minic_parser.ml"
             in
            _menhir_goto_bool_op _menhir_env _menhir_stack _menhir_s _v
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
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | DOTS2 | END | RPAR | SEMI | SEPARATOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 177 "minic_parser.mly"
                                  (Gt(a,b))
# 2343 "minic_parser.ml"
             in
            _menhir_goto_bool_op _menhir_env _menhir_stack _menhir_s _v
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
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | DOTS2 | END | RPAR | SEMI | SEPARATOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 179 "minic_parser.mly"
                                   (Geqt(a,b))
# 2400 "minic_parser.ml"
             in
            _menhir_goto_bool_op _menhir_env _menhir_stack _menhir_s _v
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
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | DOTS2 | END | RPAR | SEMI | SEPARATOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 180 "minic_parser.mly"
                                  (Eq(a,b))
# 2457 "minic_parser.ml"
             in
            _menhir_goto_bool_op _menhir_env _menhir_stack _menhir_s _v
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
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | DOTS2 | END | RPAR | SEMI | SEPARATOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 195 "minic_parser.mly"
                                   (Div(a,b))
# 2514 "minic_parser.ml"
             in
            _menhir_goto_int_op _menhir_env _menhir_stack _menhir_s _v
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
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | DOTS2 | END | RPAR | SEMI | SEPARATOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 187 "minic_parser.mly"
                                    (BXor(a,b))
# 2571 "minic_parser.ml"
             in
            _menhir_goto_bool_op _menhir_env _menhir_stack _menhir_s _v
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
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | DOTS2 | END | RPAR | SEMI | SEPARATOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 185 "minic_parser.mly"
                                   (BOr(a,b))
# 2628 "minic_parser.ml"
             in
            _menhir_goto_bool_op _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | DOTS2 | END | RPAR | SEMI | SEPARATOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 189 "minic_parser.mly"
                                    (BNeq(a,b))
# 2685 "minic_parser.ml"
             in
            _menhir_goto_bool_op _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | DOTS2 | END | RPAR | SEMI | SEPARATOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 183 "minic_parser.mly"
                                    (BAnd(a,b))
# 2742 "minic_parser.ml"
             in
            _menhir_goto_bool_op _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | DOTS2 | END | RPAR | SEMI | SEPARATOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 182 "minic_parser.mly"
                                   (And(a,b))
# 2799 "minic_parser.ml"
             in
            _menhir_goto_bool_op _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | DOTS2 | END | RPAR | SEMI | SEPARATOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 193 "minic_parser.mly"
                                   (Add(a,b))
# 2856 "minic_parser.ml"
             in
            _menhir_goto_int_op _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Minic_ast.expr))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Minic_ast.expr) = 
# 205 "minic_parser.mly"
                           (e)
# 2912 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | SUB ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | RPAR | SEMI | SEPARATOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (i : (
# 10 "minic_parser.mly"
       (string)
# 2971 "minic_parser.ml"
            ))), _, (v : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.typ * string * Minic_ast.expr) = 
# 71 "minic_parser.mly"
                                   ( (None, i, v) )
# 2977 "minic_parser.ml"
             in
            _menhir_goto_variable_set _menhir_env _menhir_stack _menhir_s _v
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
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | RPAR | SEMI | SEPARATOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (t : (Minic_ast.typ))), (i : (
# 10 "minic_parser.mly"
       (string)
# 3032 "minic_parser.ml"
            ))), _, (v : (Minic_ast.expr))) = _menhir_stack in
            let _3 = () in
            let _v : (Minic_ast.typ * string * Minic_ast.expr) = 
# 69 "minic_parser.mly"
                                           ( (t, i, v) )
# 3038 "minic_parser.ml"
             in
            _menhir_goto_variable_set _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState85 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
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
                | ADD ->
                    _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState89
                | BEGIN ->
                    _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState89
                | BOOL ->
                    _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState89
                | BREAK ->
                    _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState89
                | CONTINUE ->
                    _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState89
                | DO ->
                    _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState89
                | FOR ->
                    _menhir_run118 _menhir_env (Obj.magic _menhir_stack) MenhirState89
                | IDENT _v ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
                | IF ->
                    _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState89
                | INT ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState89
                | PUTCHAR ->
                    _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState89
                | RETURN ->
                    _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState89
                | SUB ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState89
                | SWITCH ->
                    _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState89
                | VOID ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState89
                | WHILE ->
                    _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState89
                | END ->
                    _menhir_reduce56 _menhir_env (Obj.magic _menhir_stack) MenhirState89
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState89)
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
# 122 "minic_parser.mly"
                                        ( While(c, []) )
# 3144 "minic_parser.ml"
                 in
                _menhir_goto_loop _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | SUB ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
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
                | CASE ->
                    _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState94
                | DEFAULT | END ->
                    _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState94
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState94)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | SUB ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState95 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | DOTS2 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CASE ->
                _menhir_run95 _menhir_env (Obj.magic _menhir_stack) MenhirState97
            | ADD | BEGIN | BOOL | BREAK | CONTINUE | DEFAULT | DO | END | FOR | IDENT _ | IF | INT | PUTCHAR | RETURN | SUB | SWITCH | VOID | WHILE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _, (e : (Minic_ast.expr))) = _menhir_stack in
                let _3 = () in
                let _1 = () in
                let _v : (Minic_ast.expr list) = 
# 108 "minic_parser.mly"
                            ( [e] )
# 3268 "minic_parser.ml"
                 in
                _menhir_goto_expr_case_list _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState97)
        | EQ ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState102 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
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
# 153 "minic_parser.mly"
                                         ( Return(e) )
# 3339 "minic_parser.ml"
                 in
                _menhir_goto_return _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | EQ ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Minic_ast.expr))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Minic_ast.instr) = 
# 151 "minic_parser.mly"
                               ( Return(e) )
# 3423 "minic_parser.ml"
             in
            _menhir_goto_return _menhir_env _menhir_stack _menhir_s _v
        | SUB ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState109 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
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
# 164 "minic_parser.mly"
                                        (Putchar(e))
# 3490 "minic_parser.ml"
                 in
                _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | SUB ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState114 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ADD ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | BEGIN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState116 in
                let _menhir_stack = (_menhir_stack, _menhir_s) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | ADD ->
                    _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState166
                | BEGIN ->
                    _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState166
                | BOOL ->
                    _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState166
                | BREAK ->
                    _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState166
                | CONTINUE ->
                    _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState166
                | DO ->
                    _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState166
                | FOR ->
                    _menhir_run118 _menhir_env (Obj.magic _menhir_stack) MenhirState166
                | IDENT _v ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState166 _v
                | IF ->
                    _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState166
                | INT ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState166
                | PUTCHAR ->
                    _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState166
                | RETURN ->
                    _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState166
                | SUB ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState166
                | SWITCH ->
                    _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState166
                | VOID ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState166
                | WHILE ->
                    _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState166
                | END ->
                    _menhir_reduce56 _menhir_env (Obj.magic _menhir_stack) MenhirState166
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState166)
            | BOOL ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | BREAK ->
                _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | CONTINUE ->
                _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | DO ->
                _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | FOR ->
                _menhir_run118 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | IDENT _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
            | IF ->
                _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | INT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | PUTCHAR ->
                _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | RETURN ->
                _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | SEMI ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState116 in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _, (c : (Minic_ast.expr))) = _menhir_stack in
                let _5 = () in
                let _4 = () in
                let _2 = () in
                let _1 = () in
                let _v : (Minic_ast.instr) = 
# 142 "minic_parser.mly"
                                     ( If(c, [], []) )
# 3631 "minic_parser.ml"
                 in
                _menhir_goto_conditional _menhir_env _menhir_stack _menhir_s _v
            | SUB ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | SWITCH ->
                _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | VOID ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | WHILE ->
                _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState116)
        | SUB ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState153 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SEMI ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((_menhir_stack, _menhir_s), _, (s : (Minic_ast.seq))), _, (c : (Minic_ast.expr))) = _menhir_stack in
                let _9 = () in
                let _8 = () in
                let _6 = () in
                let _5 = () in
                let _4 = () in
                let _2 = () in
                let _1 = () in
                let _v : (Minic_ast.instr) = 
# 125 "minic_parser.mly"
                                                                           ( DoWhile(s,c) )
# 3713 "minic_parser.ml"
                 in
                _menhir_goto_loop _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | SUB ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState126 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ADD ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState160
            | BOOL ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState160
            | IDENT _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _v
            | INT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState160
            | SUB ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState160
            | VOID ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState160
            | RPAR ->
                _menhir_reduce32 _menhir_env (Obj.magic _menhir_stack) MenhirState160
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState160)
        | SUB ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce71 : _menhir_env -> ('ttv_tail * _menhir_state * (Minic_ast.typ)) * (
# 10 "minic_parser.mly"
       (string)
# 3808 "minic_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s, (t : (Minic_ast.typ))), (i : (
# 10 "minic_parser.mly"
       (string)
# 3814 "minic_parser.ml"
    ))) = _menhir_stack in
    let _v : (Minic_ast.typ * string * Minic_ast.expr) = 
# 64 "minic_parser.mly"
                      ( (t, i, Undef) )
# 3819 "minic_parser.ml"
     in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState79 | MenhirState75 ->
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
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState79
            | INT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState79
            | VOID ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState79
            | RPAR ->
                _menhir_reduce63 _menhir_env (Obj.magic _menhir_stack) MenhirState79
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79)
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (d : (Minic_ast.typ * string * Minic_ast.expr))) = _menhir_stack in
            let _v : ((Minic_ast.typ * string) list) = 
# 87 "minic_parser.mly"
                      ( let (t, i, _) = d in [(t,i)] )
# 3851 "minic_parser.ml"
             in
            _menhir_goto_parameter_list _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState191 | MenhirState0 | MenhirState83 | MenhirState89 | MenhirState181 | MenhirState99 | MenhirState174 | MenhirState116 | MenhirState170 | MenhirState166 | MenhirState163 | MenhirState130 | MenhirState132 | MenhirState147 | MenhirState137 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (decl : (Minic_ast.typ * string * Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.typ * string * Minic_ast.expr) = 
# 82 "minic_parser.mly"
                                ( decl )
# 3874 "minic_parser.ml"
             in
            _menhir_goto_variable_decl_set _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run73 : _menhir_env -> ('ttv_tail * _menhir_state * (Minic_ast.typ)) * (
# 10 "minic_parser.mly"
       (string)
# 3889 "minic_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | CST _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | IDENT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | LPAR ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73

and _menhir_reduce63 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((Minic_ast.typ * string) list) = 
# 86 "minic_parser.mly"
    ( [] )
# 3913 "minic_parser.ml"
     in
    _menhir_goto_parameter_list _menhir_env _menhir_stack _menhir_s _v

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_for_seq : _menhir_env -> 'ttv_tail -> _menhir_state -> (Minic_ast.seq) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState121 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (set : (Minic_ast.typ * string * Minic_ast.expr))), _, (fs : (Minic_ast.seq))) = _menhir_stack in
        let _2 = () in
        let _v : (Minic_ast.seq) = 
# 104 "minic_parser.mly"
                                            (Set(set)::fs)
# 3934 "minic_parser.ml"
         in
        _menhir_goto_for_seq _menhir_env _menhir_stack _menhir_s _v
    | MenhirState119 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL_CST _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
            | CST _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
            | IDENT _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
            | LPAR ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | SEMI ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState126 in
                let _menhir_stack = (_menhir_stack, _menhir_s) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | ADD ->
                    _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState127
                | BOOL ->
                    _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState127
                | IDENT _v ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _v
                | INT ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState127
                | SUB ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState127
                | VOID ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState127
                | RPAR ->
                    _menhir_reduce32 _menhir_env (Obj.magic _menhir_stack) MenhirState127
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState127)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState126)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState127 ->
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
                | ADD ->
                    _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState130
                | BEGIN ->
                    _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState130
                | BOOL ->
                    _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState130
                | BREAK ->
                    _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState130
                | CONTINUE ->
                    _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState130
                | DO ->
                    _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState130
                | FOR ->
                    _menhir_run118 _menhir_env (Obj.magic _menhir_stack) MenhirState130
                | IDENT _v ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _v
                | IF ->
                    _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState130
                | INT ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState130
                | PUTCHAR ->
                    _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState130
                | RETURN ->
                    _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState130
                | SUB ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState130
                | SWITCH ->
                    _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState130
                | VOID ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState130
                | WHILE ->
                    _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState130
                | END ->
                    _menhir_reduce56 _menhir_env (Obj.magic _menhir_stack) MenhirState130
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState130)
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
    | MenhirState160 ->
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
                | ADD ->
                    _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState163
                | BEGIN ->
                    _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState163
                | BOOL ->
                    _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState163
                | BREAK ->
                    _menhir_run135 _menhir_env (Obj.magic _menhir_stack) MenhirState163
                | CONTINUE ->
                    _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState163
                | DO ->
                    _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState163
                | FOR ->
                    _menhir_run118 _menhir_env (Obj.magic _menhir_stack) MenhirState163
                | IDENT _v ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
                | IF ->
                    _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState163
                | INT ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState163
                | PUTCHAR ->
                    _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState163
                | RETURN ->
                    _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState163
                | SUB ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState163
                | SWITCH ->
                    _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState163
                | VOID ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState163
                | WHILE ->
                    _menhir_run84 _menhir_env (Obj.magic _menhir_stack) MenhirState163
                | END ->
                    _menhir_reduce56 _menhir_env (Obj.magic _menhir_stack) MenhirState163
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState163)
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

and _menhir_reduce32 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Minic_ast.seq) = 
# 102 "minic_parser.mly"
    ( [] )
# 4128 "minic_parser.ml"
     in
    _menhir_goto_for_seq _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_variable_decl_set : _menhir_env -> 'ttv_tail -> _menhir_state -> (Minic_ast.typ * string * Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState191 | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (vd : (Minic_ast.typ * string * Minic_ast.expr)) = _v in
        let _v : (Minic_ast.global_scope_def) = 
# 53 "minic_parser.mly"
                           (Variable(vd))
# 4142 "minic_parser.ml"
         in
        _menhir_goto_global_scope_def _menhir_env _menhir_stack _menhir_s _v
    | MenhirState83 | MenhirState89 | MenhirState181 | MenhirState99 | MenhirState174 | MenhirState116 | MenhirState170 | MenhirState166 | MenhirState163 | MenhirState130 | MenhirState132 | MenhirState147 | MenhirState137 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (decl : (Minic_ast.typ * string * Minic_ast.expr)) = _v in
        let _v : (Minic_ast.instr) = 
# 162 "minic_parser.mly"
                             ( Set(decl) )
# 4152 "minic_parser.ml"
         in
        _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_program : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 33 "minic_parser.mly"
      (Minic_ast.prog)
# 4161 "minic_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 33 "minic_parser.mly"
      (Minic_ast.prog)
# 4169 "minic_parser.ml"
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
# 33 "minic_parser.mly"
      (Minic_ast.prog)
# 4190 "minic_parser.ml"
            ) = 
# 38 "minic_parser.mly"
                                  (p)
# 4194 "minic_parser.ml"
             in
            _menhir_goto_program _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState191 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (d : (Minic_ast.global_scope_def))), _, (l : (Minic_ast.prog))) = _menhir_stack in
        let _v : (Minic_ast.prog) = 
# 49 "minic_parser.mly"
                                                 ( d::l )
# 4210 "minic_parser.ml"
         in
        _menhir_goto_global_scope_def_list _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
    | CST _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
    | IDENT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
    | LPAR ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState11

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 10 "minic_parser.mly"
       (string)
# 4238 "minic_parser.ml"
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
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
        | CST _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
        | IDENT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
        | LPAR ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState13
        | RPAR ->
            _menhir_reduce35 _menhir_env (Obj.magic _menhir_stack) MenhirState13
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState13)
    | ADD | AND | BAND | BNEQ | BOR | BXOR | DIV | DOTS2 | END | EQ | GET | GT | LET | LT | MOD | MUL | NEQ | OR | RPAR | SEMI | SEPARATOR | SUB | XOR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (i : (
# 10 "minic_parser.mly"
       (string)
# 4269 "minic_parser.ml"
        ))) = _menhir_stack in
        let _v : (Minic_ast.expr) = 
# 204 "minic_parser.mly"
            (Get(i))
# 4274 "minic_parser.ml"
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
# 4287 "minic_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (n : (
# 8 "minic_parser.mly"
       (int)
# 4295 "minic_parser.ml"
    )) = _v in
    let _v : (Minic_ast.expr) = 
# 202 "minic_parser.mly"
          (Cst(n))
# 4300 "minic_parser.ml"
     in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run15 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 9 "minic_parser.mly"
       (bool)
# 4307 "minic_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (b : (
# 9 "minic_parser.mly"
       (bool)
# 4315 "minic_parser.ml"
    )) = _v in
    let _v : (Minic_ast.expr) = 
# 203 "minic_parser.mly"
               (BCst(b))
# 4320 "minic_parser.ml"
     in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_typ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Minic_ast.typ) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState191 | MenhirState0 ->
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
                    _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState75
                | INT ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState75
                | VOID ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState75
                | RPAR ->
                    _menhir_reduce63 _menhir_env (Obj.magic _menhir_stack) MenhirState75
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75)
            | SET ->
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
            | SEMI ->
                _menhir_reduce71 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState79 | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            _menhir_reduce71 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState160 | MenhirState127 | MenhirState119 | MenhirState121 ->
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
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState83 | MenhirState89 | MenhirState181 | MenhirState99 | MenhirState174 | MenhirState116 | MenhirState170 | MenhirState166 | MenhirState163 | MenhirState130 | MenhirState132 | MenhirState147 | MenhirState137 ->
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
                _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
            | SEMI ->
                _menhir_reduce71 _menhir_env (Obj.magic _menhir_stack)
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

and _menhir_goto_variable_set : _menhir_env -> 'ttv_tail -> _menhir_state -> (Minic_ast.typ * string * Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState191 | MenhirState83 | MenhirState89 | MenhirState181 | MenhirState99 | MenhirState174 | MenhirState116 | MenhirState170 | MenhirState166 | MenhirState163 | MenhirState130 | MenhirState132 | MenhirState147 | MenhirState137 | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (set : (Minic_ast.typ * string * Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.typ * string * Minic_ast.expr) = 
# 81 "minic_parser.mly"
                            ( set )
# 4461 "minic_parser.ml"
             in
            _menhir_goto_variable_decl_set _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState160 | MenhirState127 | MenhirState121 | MenhirState119 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SEPARATOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ADD ->
                _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | BOOL ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | IDENT _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
            | INT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | SUB ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | VOID ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | RPAR | SEMI ->
                _menhir_reduce32 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState121)
        | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (set : (Minic_ast.typ * string * Minic_ast.expr))) = _menhir_stack in
            let _v : (Minic_ast.seq) = 
# 103 "minic_parser.mly"
                     ([Set(set)])
# 4504 "minic_parser.ml"
             in
            _menhir_goto_for_seq _menhir_env _menhir_stack _menhir_s _v
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
    | MenhirState191 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState181 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState176 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState174 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState170 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState166 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState163 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState160 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState153 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState147 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState137 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState132 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState130 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState127 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState126 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState121 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState119 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState116 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState114 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState109 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState102 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState97 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState95 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState85 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
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
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState10 ->
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
# 33 "minic_parser.mly"
      (Minic_ast.prog)
# 4750 "minic_parser.ml"
        ) = let _startpos = _startpos__1_ in
        
# 40 "minic_parser.mly"
    ( 
        let pos = _startpos in
        let message = Printf.sprintf "Syntax error at line %d:%d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol) in 
        failwith message 
    )
# 4759 "minic_parser.ml"
         in
        _menhir_goto_program _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce41 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Minic_ast.prog) = 
# 48 "minic_parser.mly"
    ( [] )
# 4768 "minic_parser.ml"
     in
    _menhir_goto_global_scope_def_list _menhir_env _menhir_stack _menhir_s _v

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Minic_ast.typ) = 
# 60 "minic_parser.mly"
         ( Void )
# 4780 "minic_parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SUB ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (i : (
# 10 "minic_parser.mly"
       (string)
# 4802 "minic_parser.ml"
            )) = _v in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            let _2 = () in
            let _1 = () in
            let _v : (Minic_ast.typ * string * Minic_ast.expr) = 
# 77 "minic_parser.mly"
                      ( (None, i, Add(Get(i),Cst(1))) )
# 4810 "minic_parser.ml"
             in
            _menhir_goto_variable_set _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Minic_ast.typ) = 
# 58 "minic_parser.mly"
        ( Int )
# 4834 "minic_parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 10 "minic_parser.mly"
       (string)
# 4841 "minic_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADD ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (i : (
# 10 "minic_parser.mly"
       (string)
# 4860 "minic_parser.ml"
            ))) = _menhir_stack in
            let _3 = () in
            let _2 = () in
            let _v : (Minic_ast.typ * string * Minic_ast.expr) = 
# 73 "minic_parser.mly"
                      ( (None, i, Add(Get(i),Cst(1))) )
# 4867 "minic_parser.ml"
             in
            _menhir_goto_variable_set _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | SET ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BOOL_CST _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
        | CST _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
        | IDENT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
        | LPAR ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState10
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState10)
    | SUB ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SUB ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (i : (
# 10 "minic_parser.mly"
       (string)
# 4905 "minic_parser.ml"
            ))) = _menhir_stack in
            let _3 = () in
            let _2 = () in
            let _v : (Minic_ast.typ * string * Minic_ast.expr) = 
# 76 "minic_parser.mly"
                      ( (None, i, Add(Get(i),Cst(1))) )
# 4912 "minic_parser.ml"
             in
            _menhir_goto_variable_set _menhir_env _menhir_stack _menhir_s _v
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
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run64 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Minic_ast.typ) = 
# 59 "minic_parser.mly"
         ( Bool )
# 4936 "minic_parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run65 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADD ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (i : (
# 10 "minic_parser.mly"
       (string)
# 4958 "minic_parser.ml"
            )) = _v in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            let _2 = () in
            let _1 = () in
            let _v : (Minic_ast.typ * string * Minic_ast.expr) = 
# 74 "minic_parser.mly"
                      ( (None, i, Add(Get(i),Cst(1))) )
# 4966 "minic_parser.ml"
             in
            _menhir_goto_variable_set _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

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
# 33 "minic_parser.mly"
      (Minic_ast.prog)
# 4997 "minic_parser.ml"
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
    | ADD ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | BOOL ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | IDENT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | INT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | SUB ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | VOID ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | EOF ->
        _menhir_reduce41 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

# 269 "<standard.mly>"
  

# 5033 "minic_parser.ml"
