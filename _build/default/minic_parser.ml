
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
  | MenhirState164
  | MenhirState155
  | MenhirState151
  | MenhirState147
  | MenhirState144
  | MenhirState141
  | MenhirState134
  | MenhirState129
  | MenhirState121
  | MenhirState120
  | MenhirState118
  | MenhirState115
  | MenhirState114
  | MenhirState109
  | MenhirState107
  | MenhirState104
  | MenhirState102
  | MenhirState97
  | MenhirState90
  | MenhirState88
  | MenhirState87
  | MenhirState83
  | MenhirState81
  | MenhirState77
  | MenhirState73
  | MenhirState71
  | MenhirState53
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
  | MenhirState13
  | MenhirState11
  | MenhirState10
  | MenhirState0

# 1 "minic_parser.mly"
  
  open Lexing
  open Minic_ast

# 136 "minic_parser.ml"

let rec _menhir_goto_list_instruction_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Minic_ast.seq) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState121 ->
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
# 146 "minic_parser.mly"
                                      ( Scope(s) )
# 157 "minic_parser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState129 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Minic_ast.instr))), _, (xs : (Minic_ast.seq))) = _menhir_stack in
        let _v : (Minic_ast.seq) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 173 "minic_parser.ml"
         in
        _menhir_goto_list_instruction_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState120 ->
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
                        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _v
                    | CST _v ->
                        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _v
                    | IDENT _v ->
                        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _v
                    | LPAR ->
                        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState134
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState134)
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
    | MenhirState118 ->
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
# 142 "minic_parser.mly"
                                                                                                    ( For(init_instr, BCst(true), incr_instr, s) )
# 246 "minic_parser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState144 ->
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
# 141 "minic_parser.mly"
                                                                                                                      ( For(init_instr, cond, incr_instr, s) )
# 275 "minic_parser.ml"
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
                        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState151
                    | BEGIN ->
                        _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState151
                    | BOOL ->
                        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState151
                    | DO ->
                        _menhir_run119 _menhir_env (Obj.magic _menhir_stack) MenhirState151
                    | FOR ->
                        _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState151
                    | IDENT _v ->
                        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _v
                    | IF ->
                        _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState151
                    | INT ->
                        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState151
                    | PUTCHAR ->
                        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState151
                    | RETURN ->
                        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState151
                    | SUB ->
                        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState151
                    | VOID ->
                        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState151
                    | WHILE ->
                        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState151
                    | END ->
                        _menhir_reduce51 _menhir_env (Obj.magic _menhir_stack) MenhirState151
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState151)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | ADD | BEGIN | BOOL | DO | END | FOR | IDENT _ | IF | INT | PUTCHAR | RETURN | SUB | VOID | WHILE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((((_menhir_stack, _menhir_s), _, (c : (Minic_ast.expr))), _), _, (s : (Minic_ast.seq))) = _menhir_stack in
                let _7 = () in
                let _5 = () in
                let _4 = () in
                let _2 = () in
                let _1 = () in
                let _v : (Minic_ast.instr) = 
# 125 "minic_parser.mly"
                                                                ( If(c, s, []) )
# 353 "minic_parser.ml"
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
    | MenhirState151 ->
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
# 123 "minic_parser.mly"
                                                                                                       ( If(c,s1,s2) )
# 389 "minic_parser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
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
# 134 "minic_parser.mly"
                                                                   ( While(c,s) )
# 416 "minic_parser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState81 ->
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
# 437 "minic_parser.ml"
            ))), _, (p : ((Minic_ast.typ * string) list))), _, (s : (Minic_ast.seq))) = _menhir_stack in
            let _8 = () in
            let _6 = () in
            let _5 = () in
            let _3 = () in
            let _v : (Minic_ast.fun_def) = 
# 100 "minic_parser.mly"
    ( { 
        name = f;
        code = s;
        params = p;
        return = t;
    } )
# 451 "minic_parser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (fd : (Minic_ast.fun_def)) = _v in
            let _v : (Minic_ast.global_scope_def) = 
# 54 "minic_parser.mly"
                       (Function(fd))
# 459 "minic_parser.ml"
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

and _menhir_goto_call_list : _menhir_env -> 'ttv_tail -> _menhir_state -> (Minic_ast.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (e : (Minic_ast.expr))), _, (cl : (Minic_ast.expr list))) = _menhir_stack in
        let _2 = () in
        let _v : (Minic_ast.expr list) = 
# 154 "minic_parser.mly"
                                        ( e::cl )
# 483 "minic_parser.ml"
         in
        _menhir_goto_call_list _menhir_env _menhir_stack _menhir_s _v
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
# 498 "minic_parser.ml"
            ))), _, (p : (Minic_ast.expr list))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 158 "minic_parser.mly"
                                  (Call(f, p))
# 505 "minic_parser.ml"
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
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
    | LPAR ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState17
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
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | LPAR ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState19
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
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
    | LPAR ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState21
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
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | LPAR ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState23
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
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | LPAR ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState25
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
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | LPAR ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState27
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
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | LPAR ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState29
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
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | LPAR ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState31
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
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | LPAR ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState33
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
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | LPAR ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState35
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
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | LPAR ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState37
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
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | LPAR ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState39
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
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | LPAR ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState41
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
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | LPAR ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState43
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
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | LPAR ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState45
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
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | LPAR ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState47
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
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | LPAR ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState49
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
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | LPAR ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51

and _menhir_goto_parameter_list : _menhir_env -> 'ttv_tail -> _menhir_state -> ((Minic_ast.typ * string) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (d : (Minic_ast.typ * string * Minic_ast.expr))), _, (p : ((Minic_ast.typ * string) list))) = _menhir_stack in
        let _2 = () in
        let _v : ((Minic_ast.typ * string) list) = 
# 94 "minic_parser.mly"
                                                 ( let (t, i, _) = d in (t,i)::p )
# 853 "minic_parser.ml"
         in
        _menhir_goto_parameter_list _menhir_env _menhir_stack _menhir_s _v
    | MenhirState73 ->
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
                    _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState81
                | BEGIN ->
                    _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState81
                | BOOL ->
                    _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState81
                | DO ->
                    _menhir_run119 _menhir_env (Obj.magic _menhir_stack) MenhirState81
                | FOR ->
                    _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState81
                | IDENT _v ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
                | IF ->
                    _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState81
                | INT ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState81
                | PUTCHAR ->
                    _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState81
                | RETURN ->
                    _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState81
                | SUB ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState81
                | VOID ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState81
                | WHILE ->
                    _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState81
                | END ->
                    _menhir_reduce51 _menhir_env (Obj.magic _menhir_stack) MenhirState81
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81)
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

and _menhir_reduce51 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Minic_ast.seq) = 
# 211 "<standard.mly>"
    ( [] )
# 923 "minic_parser.ml"
     in
    _menhir_goto_list_instruction_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run82 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
        | CST _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
        | IDENT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
        | LPAR ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run88 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState88 in
        let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BOOL_CST _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
        | CST _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
        | IDENT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
        | LPAR ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90)
    | BOOL_CST _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | CST _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | IDENT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | LPAR ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState88
    | SEMI ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState88 in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : (Minic_ast.instr) = 
# 120 "minic_parser.mly"
                  ( Return(Undef) )
# 1001 "minic_parser.ml"
         in
        _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState88

and _menhir_run96 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
        | CST _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
        | IDENT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
        | LPAR ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState97
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState97)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run101 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run106 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | BOOL ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | IDENT _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
        | INT ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | SUB ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | VOID ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | SEMI ->
            _menhir_reduce27 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState107)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run119 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | BEGIN ->
            _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | BOOL ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | DO ->
            _menhir_run119 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | FOR ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | IDENT _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
        | IF ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | INT ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | PUTCHAR ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | RETURN ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | SUB ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | VOID ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | WHILE ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | END ->
            _menhir_reduce51 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState120)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run121 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADD ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | BEGIN ->
        _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | BOOL ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | DO ->
        _menhir_run119 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | FOR ->
        _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | IDENT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
    | IF ->
        _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | INT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | PUTCHAR ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | RETURN ->
        _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | SUB ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | VOID ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | WHILE ->
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | END ->
        _menhir_reduce51 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState121

and _menhir_goto_instruction : _menhir_env -> 'ttv_tail -> _menhir_state -> (Minic_ast.instr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState81 | MenhirState87 | MenhirState147 | MenhirState151 | MenhirState144 | MenhirState118 | MenhirState120 | MenhirState129 | MenhirState121 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | BEGIN ->
            _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | BOOL ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | DO ->
            _menhir_run119 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | FOR ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | IDENT _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState129 _v
        | IF ->
            _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | INT ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | PUTCHAR ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | RETURN ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | SUB ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | VOID ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | WHILE ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | END ->
            _menhir_reduce51 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState129)
    | MenhirState104 ->
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
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState155
            | BEGIN ->
                _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState155
            | BOOL ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState155
            | DO ->
                _menhir_run119 _menhir_env (Obj.magic _menhir_stack) MenhirState155
            | FOR ->
                _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState155
            | IDENT _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
            | IF ->
                _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState155
            | INT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState155
            | PUTCHAR ->
                _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState155
            | RETURN ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState155
            | SUB ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState155
            | VOID ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState155
            | WHILE ->
                _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState155
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState155)
        | ADD | BEGIN | BOOL | DO | END | FOR | IDENT _ | IF | INT | PUTCHAR | RETURN | SUB | VOID | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (c : (Minic_ast.expr))), _, (i : (Minic_ast.instr))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Minic_ast.instr) = 
# 129 "minic_parser.mly"
                                                 ( If(c, [i], []) )
# 1284 "minic_parser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState155 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s), _, (c : (Minic_ast.expr))), _, (i1 : (Minic_ast.instr))), _, (i2 : (Minic_ast.instr))) = _menhir_stack in
        let _6 = () in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _v : (Minic_ast.instr) = 
# 127 "minic_parser.mly"
                                                                       ( If(c, [i1], [i2]) )
# 1304 "minic_parser.ml"
         in
        _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
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
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState164
    | BOOL ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState164
    | IDENT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _v
    | INT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState164
    | SUB ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState164
    | VOID ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState164
    | EOF ->
        _menhir_reduce33 _menhir_env (Obj.magic _menhir_stack) MenhirState164
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState164

and _menhir_reduce1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Minic_ast.expr list) = 
# 152 "minic_parser.mly"
    ( [] )
# 1341 "minic_parser.ml"
     in
    _menhir_goto_call_list _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState53 | MenhirState13 ->
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
        | SEPARATOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL_CST _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
            | CST _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
            | IDENT _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
            | LPAR ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState53
            | RPAR ->
                _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState53
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53)
        | SUB ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (e : (Minic_ast.expr))) = _menhir_stack in
            let _v : (Minic_ast.expr list) = 
# 153 "minic_parser.mly"
                 ( [e] )
# 1415 "minic_parser.ml"
             in
            _menhir_goto_call_list _menhir_env _menhir_stack _menhir_s _v
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
        | END | RPAR | SEMI | SEPARATOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 174 "minic_parser.mly"
                                     (Xor(a,b))
# 1472 "minic_parser.ml"
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
        | END | RPAR | SEMI | SEPARATOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 183 "minic_parser.mly"
                                     (Sub(a,b))
# 1529 "minic_parser.ml"
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
        | END | RPAR | SEMI | SEPARATOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 172 "minic_parser.mly"
                                    (Or(a,b))
# 1586 "minic_parser.ml"
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
        | END | RPAR | SEMI | SEPARATOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 176 "minic_parser.mly"
                                     (Neq(a,b))
# 1643 "minic_parser.ml"
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
        | END | RPAR | SEMI | SEPARATOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 180 "minic_parser.mly"
                                     (Mul(a,b))
# 1700 "minic_parser.ml"
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
        | END | RPAR | SEMI | SEPARATOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 182 "minic_parser.mly"
                                     (Mod(a,b))
# 1757 "minic_parser.ml"
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
        | END | RPAR | SEMI | SEPARATOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 164 "minic_parser.mly"
                                    (Lt(a,b))
# 1814 "minic_parser.ml"
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
        | END | RPAR | SEMI | SEPARATOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 166 "minic_parser.mly"
                                     (Leqt(a,b))
# 1871 "minic_parser.ml"
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
        | END | RPAR | SEMI | SEPARATOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 165 "minic_parser.mly"
                                    (Gt(a,b))
# 1928 "minic_parser.ml"
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
        | END | RPAR | SEMI | SEPARATOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 167 "minic_parser.mly"
                                     (Geqt(a,b))
# 1985 "minic_parser.ml"
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
        | END | RPAR | SEMI | SEPARATOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 168 "minic_parser.mly"
                                    (Eq(a,b))
# 2042 "minic_parser.ml"
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
        | END | RPAR | SEMI | SEPARATOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 181 "minic_parser.mly"
                                     (Div(a,b))
# 2099 "minic_parser.ml"
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
        | END | RPAR | SEMI | SEPARATOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 175 "minic_parser.mly"
                                      (BXor(a,b))
# 2156 "minic_parser.ml"
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
        | END | RPAR | SEMI | SEPARATOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 173 "minic_parser.mly"
                                     (BOr(a,b))
# 2213 "minic_parser.ml"
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
        | END | RPAR | SEMI | SEPARATOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 177 "minic_parser.mly"
                                      (BNeq(a,b))
# 2270 "minic_parser.ml"
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
        | END | RPAR | SEMI | SEPARATOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 171 "minic_parser.mly"
                                      (BAnd(a,b))
# 2327 "minic_parser.ml"
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
        | END | RPAR | SEMI | SEPARATOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 170 "minic_parser.mly"
                                     (And(a,b))
# 2384 "minic_parser.ml"
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
        | END | RPAR | SEMI | SEPARATOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 179 "minic_parser.mly"
                                     (Add(a,b))
# 2441 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
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
# 162 "minic_parser.mly"
                           ( e )
# 2497 "minic_parser.ml"
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
    | MenhirState10 ->
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
        | RPAR | SEMI | SEPARATOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (i : (
# 10 "minic_parser.mly"
       (string)
# 2556 "minic_parser.ml"
            ))), _, (v : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.typ * string * Minic_ast.expr) = 
# 75 "minic_parser.mly"
                                   ( (None, i, v) )
# 2562 "minic_parser.ml"
             in
            _menhir_goto_variable_set _menhir_env _menhir_stack _menhir_s _v
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
        | RPAR | SEMI | SEPARATOR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (t : (Minic_ast.typ))), (i : (
# 10 "minic_parser.mly"
       (string)
# 2617 "minic_parser.ml"
            ))), _, (v : (Minic_ast.expr))) = _menhir_stack in
            let _3 = () in
            let _v : (Minic_ast.typ * string * Minic_ast.expr) = 
# 73 "minic_parser.mly"
                                           ( (t, i, v) )
# 2623 "minic_parser.ml"
             in
            _menhir_goto_variable_set _menhir_env _menhir_stack _menhir_s _v
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
                | ADD ->
                    _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState87
                | BEGIN ->
                    _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState87
                | BOOL ->
                    _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState87
                | DO ->
                    _menhir_run119 _menhir_env (Obj.magic _menhir_stack) MenhirState87
                | FOR ->
                    _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState87
                | IDENT _v ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
                | IF ->
                    _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState87
                | INT ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState87
                | PUTCHAR ->
                    _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState87
                | RETURN ->
                    _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState87
                | SUB ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState87
                | VOID ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState87
                | WHILE ->
                    _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState87
                | END ->
                    _menhir_reduce51 _menhir_env (Obj.magic _menhir_stack) MenhirState87
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87)
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
# 136 "minic_parser.mly"
                                        ( While(c, []) )
# 2723 "minic_parser.ml"
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
    | MenhirState90 ->
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
# 118 "minic_parser.mly"
                                         ( Return(e) )
# 2778 "minic_parser.ml"
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
    | MenhirState88 ->
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
# 116 "minic_parser.mly"
                               ( Return(e) )
# 2862 "minic_parser.ml"
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
    | MenhirState97 ->
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
# 148 "minic_parser.mly"
                                          (Putchar(e))
# 2929 "minic_parser.ml"
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
    | MenhirState102 ->
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
            | ADD ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | BEGIN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState104 in
                let _menhir_stack = (_menhir_stack, _menhir_s) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | ADD ->
                    _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState147
                | BEGIN ->
                    _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState147
                | BOOL ->
                    _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState147
                | DO ->
                    _menhir_run119 _menhir_env (Obj.magic _menhir_stack) MenhirState147
                | FOR ->
                    _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState147
                | IDENT _v ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
                | IF ->
                    _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState147
                | INT ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState147
                | PUTCHAR ->
                    _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState147
                | RETURN ->
                    _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState147
                | SUB ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState147
                | VOID ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState147
                | WHILE ->
                    _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState147
                | END ->
                    _menhir_reduce51 _menhir_env (Obj.magic _menhir_stack) MenhirState147
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState147)
            | BOOL ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | DO ->
                _menhir_run119 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | FOR ->
                _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | IDENT _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
            | IF ->
                _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | INT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | PUTCHAR ->
                _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | RETURN ->
                _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | SEMI ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState104 in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _, (c : (Minic_ast.expr))) = _menhir_stack in
                let _5 = () in
                let _4 = () in
                let _2 = () in
                let _1 = () in
                let _v : (Minic_ast.instr) = 
# 131 "minic_parser.mly"
                                     ( If(c, [], []) )
# 3060 "minic_parser.ml"
                 in
                _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
            | SUB ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | VOID ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | WHILE ->
                _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState104)
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
    | MenhirState134 ->
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
# 139 "minic_parser.mly"
                                                                           ( DoWhile(s,c) )
# 3140 "minic_parser.ml"
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
    | MenhirState114 ->
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
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ADD ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState141
            | BOOL ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState141
            | IDENT _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _v
            | INT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState141
            | SUB ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState141
            | VOID ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState141
            | RPAR ->
                _menhir_reduce27 _menhir_env (Obj.magic _menhir_stack) MenhirState141
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState141)
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

and _menhir_reduce58 : _menhir_env -> ('ttv_tail * _menhir_state * (Minic_ast.typ)) * (
# 10 "minic_parser.mly"
       (string)
# 3235 "minic_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s, (t : (Minic_ast.typ))), (i : (
# 10 "minic_parser.mly"
       (string)
# 3241 "minic_parser.ml"
    ))) = _menhir_stack in
    let _v : (Minic_ast.typ * string * Minic_ast.expr) = 
# 68 "minic_parser.mly"
                      ( (t, i, Undef) )
# 3246 "minic_parser.ml"
     in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState77 | MenhirState73 ->
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
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | INT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | VOID ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | RPAR ->
                _menhir_reduce53 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77)
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (d : (Minic_ast.typ * string * Minic_ast.expr))) = _menhir_stack in
            let _v : ((Minic_ast.typ * string) list) = 
# 93 "minic_parser.mly"
                      ( let (t, i, _) = d in [(t,i)] )
# 3278 "minic_parser.ml"
             in
            _menhir_goto_parameter_list _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState164 | MenhirState0 | MenhirState81 | MenhirState87 | MenhirState155 | MenhirState104 | MenhirState151 | MenhirState147 | MenhirState144 | MenhirState118 | MenhirState120 | MenhirState129 | MenhirState121 ->
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
# 87 "minic_parser.mly"
                                ( decl )
# 3301 "minic_parser.ml"
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

and _menhir_run71 : _menhir_env -> ('ttv_tail * _menhir_state * (Minic_ast.typ)) * (
# 10 "minic_parser.mly"
       (string)
# 3316 "minic_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | CST _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | IDENT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | LPAR ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71

and _menhir_reduce53 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((Minic_ast.typ * string) list) = 
# 92 "minic_parser.mly"
    ( [] )
# 3340 "minic_parser.ml"
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
    | MenhirState109 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (set : (Minic_ast.typ * string * Minic_ast.expr))), _, (fs : (Minic_ast.seq))) = _menhir_stack in
        let _2 = () in
        let _v : (Minic_ast.seq) = 
# 111 "minic_parser.mly"
                                            (Set(set)::fs)
# 3361 "minic_parser.ml"
         in
        _menhir_goto_for_seq _menhir_env _menhir_stack _menhir_s _v
    | MenhirState107 ->
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
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
            | CST _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
            | IDENT _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
            | LPAR ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState114
            | SEMI ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState114 in
                let _menhir_stack = (_menhir_stack, _menhir_s) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | ADD ->
                    _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState115
                | BOOL ->
                    _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState115
                | IDENT _v ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
                | INT ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState115
                | SUB ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState115
                | VOID ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState115
                | RPAR ->
                    _menhir_reduce27 _menhir_env (Obj.magic _menhir_stack) MenhirState115
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState115)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState114)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState115 ->
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
                    _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState118
                | BEGIN ->
                    _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState118
                | BOOL ->
                    _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState118
                | DO ->
                    _menhir_run119 _menhir_env (Obj.magic _menhir_stack) MenhirState118
                | FOR ->
                    _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState118
                | IDENT _v ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
                | IF ->
                    _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState118
                | INT ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState118
                | PUTCHAR ->
                    _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState118
                | RETURN ->
                    _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState118
                | SUB ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState118
                | VOID ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState118
                | WHILE ->
                    _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState118
                | END ->
                    _menhir_reduce51 _menhir_env (Obj.magic _menhir_stack) MenhirState118
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState118)
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
    | MenhirState141 ->
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
                    _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState144
                | BEGIN ->
                    _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState144
                | BOOL ->
                    _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState144
                | DO ->
                    _menhir_run119 _menhir_env (Obj.magic _menhir_stack) MenhirState144
                | FOR ->
                    _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState144
                | IDENT _v ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _v
                | IF ->
                    _menhir_run101 _menhir_env (Obj.magic _menhir_stack) MenhirState144
                | INT ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState144
                | PUTCHAR ->
                    _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState144
                | RETURN ->
                    _menhir_run88 _menhir_env (Obj.magic _menhir_stack) MenhirState144
                | SUB ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState144
                | VOID ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState144
                | WHILE ->
                    _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState144
                | END ->
                    _menhir_reduce51 _menhir_env (Obj.magic _menhir_stack) MenhirState144
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState144)
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

and _menhir_reduce27 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Minic_ast.seq) = 
# 109 "minic_parser.mly"
    ( [] )
# 3543 "minic_parser.ml"
     in
    _menhir_goto_for_seq _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_variable_decl_set : _menhir_env -> 'ttv_tail -> _menhir_state -> (Minic_ast.typ * string * Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState164 | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (vd : (Minic_ast.typ * string * Minic_ast.expr)) = _v in
        let _v : (Minic_ast.global_scope_def) = 
# 53 "minic_parser.mly"
                           (Variable(vd))
# 3557 "minic_parser.ml"
         in
        _menhir_goto_global_scope_def _menhir_env _menhir_stack _menhir_s _v
    | MenhirState81 | MenhirState87 | MenhirState155 | MenhirState104 | MenhirState151 | MenhirState147 | MenhirState144 | MenhirState118 | MenhirState120 | MenhirState129 | MenhirState121 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (decl : (Minic_ast.typ * string * Minic_ast.expr)) = _v in
        let _v : (Minic_ast.instr) = 
# 144 "minic_parser.mly"
                               ( Set(decl) )
# 3567 "minic_parser.ml"
         in
        _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_program : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 33 "minic_parser.mly"
      (Minic_ast.prog)
# 3576 "minic_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 33 "minic_parser.mly"
      (Minic_ast.prog)
# 3584 "minic_parser.ml"
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
# 3605 "minic_parser.ml"
            ) = 
# 38 "minic_parser.mly"
                                (p)
# 3609 "minic_parser.ml"
             in
            _menhir_goto_program _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState164 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (d : (Minic_ast.global_scope_def))), _, (l : (Minic_ast.prog))) = _menhir_stack in
        let _v : (Minic_ast.prog) = 
# 49 "minic_parser.mly"
                                                 ( d::l )
# 3625 "minic_parser.ml"
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
# 3653 "minic_parser.ml"
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
            _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState13
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState13)
    | ADD | AND | BAND | BNEQ | BOR | BXOR | DIV | END | EQ | GET | GT | LET | LT | MOD | MUL | NEQ | OR | RPAR | SEMI | SEPARATOR | SUB | XOR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (i : (
# 10 "minic_parser.mly"
       (string)
# 3684 "minic_parser.ml"
        ))) = _menhir_stack in
        let _v : (Minic_ast.expr) = 
# 161 "minic_parser.mly"
            ( Get(i) )
# 3689 "minic_parser.ml"
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
# 3702 "minic_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (n : (
# 8 "minic_parser.mly"
       (int)
# 3710 "minic_parser.ml"
    )) = _v in
    let _v : (Minic_ast.expr) = 
# 159 "minic_parser.mly"
          ( Cst(n) )
# 3715 "minic_parser.ml"
     in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run15 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 9 "minic_parser.mly"
       (bool)
# 3722 "minic_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (b : (
# 9 "minic_parser.mly"
       (bool)
# 3730 "minic_parser.ml"
    )) = _v in
    let _v : (Minic_ast.expr) = 
# 160 "minic_parser.mly"
               ( BCst(b) )
# 3735 "minic_parser.ml"
     in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_typ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Minic_ast.typ) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState164 | MenhirState0 ->
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
                    _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                | INT ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                | VOID ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                | RPAR ->
                    _menhir_reduce53 _menhir_env (Obj.magic _menhir_stack) MenhirState73
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73)
            | SET ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
            | SEMI ->
                _menhir_reduce58 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState77 | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            _menhir_reduce58 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState141 | MenhirState115 | MenhirState107 | MenhirState109 ->
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
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState81 | MenhirState87 | MenhirState155 | MenhirState104 | MenhirState151 | MenhirState147 | MenhirState144 | MenhirState118 | MenhirState120 | MenhirState129 | MenhirState121 ->
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
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
            | SEMI ->
                _menhir_reduce58 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState164 | MenhirState81 | MenhirState87 | MenhirState155 | MenhirState104 | MenhirState151 | MenhirState147 | MenhirState144 | MenhirState118 | MenhirState120 | MenhirState129 | MenhirState121 | MenhirState0 ->
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
# 86 "minic_parser.mly"
                            ( set )
# 3876 "minic_parser.ml"
             in
            _menhir_goto_variable_decl_set _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState141 | MenhirState115 | MenhirState109 | MenhirState107 ->
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
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState109
            | BOOL ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState109
            | IDENT _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
            | INT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState109
            | SUB ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState109
            | VOID ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState109
            | RPAR | SEMI ->
                _menhir_reduce27 _menhir_env (Obj.magic _menhir_stack) MenhirState109
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState109)
        | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (set : (Minic_ast.typ * string * Minic_ast.expr))) = _menhir_stack in
            let _v : (Minic_ast.seq) = 
# 110 "minic_parser.mly"
                     ([Set(set)])
# 3919 "minic_parser.ml"
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
    | MenhirState164 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState155 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState151 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState147 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState144 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState141 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState134 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState129 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState121 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState120 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState115 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState114 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState109 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState107 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState104 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState102 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState97 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState88 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState53 ->
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
# 4137 "minic_parser.ml"
        ) = let _startpos = _startpos__1_ in
        
# 40 "minic_parser.mly"
  ( 
      let pos = _startpos in
      let message = Printf.sprintf "Syntax error at line %d:%d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol) in 
      failwith message 
  )
# 4146 "minic_parser.ml"
         in
        _menhir_goto_program _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce33 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Minic_ast.prog) = 
# 48 "minic_parser.mly"
    ( [] )
# 4155 "minic_parser.ml"
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
# 4167 "minic_parser.ml"
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
# 4189 "minic_parser.ml"
            )) = _v in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            let _2 = () in
            let _1 = () in
            let _v : (Minic_ast.typ * string * Minic_ast.expr) = 
# 81 "minic_parser.mly"
                      ( (None, i, Add(Get(i),Cst(1))) )
# 4197 "minic_parser.ml"
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
# 4221 "minic_parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 10 "minic_parser.mly"
       (string)
# 4228 "minic_parser.ml"
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
# 4247 "minic_parser.ml"
            ))) = _menhir_stack in
            let _3 = () in
            let _2 = () in
            let _v : (Minic_ast.typ * string * Minic_ast.expr) = 
# 77 "minic_parser.mly"
                      ( (None, i, Add(Get(i),Cst(1))) )
# 4254 "minic_parser.ml"
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
# 4292 "minic_parser.ml"
            ))) = _menhir_stack in
            let _3 = () in
            let _2 = () in
            let _v : (Minic_ast.typ * string * Minic_ast.expr) = 
# 80 "minic_parser.mly"
                      ( (None, i, Add(Get(i),Cst(1))) )
# 4299 "minic_parser.ml"
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

and _menhir_run62 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Minic_ast.typ) = 
# 59 "minic_parser.mly"
         ( Bool )
# 4323 "minic_parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run63 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
# 4345 "minic_parser.ml"
            )) = _v in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            let _2 = () in
            let _1 = () in
            let _v : (Minic_ast.typ * string * Minic_ast.expr) = 
# 78 "minic_parser.mly"
                      ( (None, i, Add(Get(i),Cst(1))) )
# 4353 "minic_parser.ml"
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
# 4384 "minic_parser.ml"
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
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | BOOL ->
        _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | IDENT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | INT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | SUB ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | VOID ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | EOF ->
        _menhir_reduce33 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

# 269 "<standard.mly>"
  

# 4420 "minic_parser.ml"
