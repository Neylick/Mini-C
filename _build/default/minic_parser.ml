
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | XOR
    | WHILE
    | SUB
    | SET
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
# 27 "minic_parser.ml"
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
# 39 "minic_parser.ml"
  )
    | BXOR
    | BOR
    | BOOL_CST of (
# 9 "minic_parser.mly"
       (bool)
# 46 "minic_parser.ml"
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
  | MenhirState74
  | MenhirState68
  | MenhirState65
  | MenhirState61
  | MenhirState59
  | MenhirState56
  | MenhirState55
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
  | MenhirState14
  | MenhirState13
  | MenhirState4
  | MenhirState0

# 1 "minic_parser.mly"
  
  open Lexing
  open Minic_ast

# 105 "minic_parser.ml"

let rec _menhir_run19 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | CST _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | IDENT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
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
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
    | CST _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
    | IDENT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
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
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | CST _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | IDENT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
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
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | CST _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | IDENT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
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
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | CST _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | IDENT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
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
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | CST _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | IDENT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
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
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | CST _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | IDENT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
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
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | CST _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | IDENT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
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
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | CST _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | IDENT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
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
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | CST _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | IDENT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
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
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | CST _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | IDENT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
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
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | CST _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | IDENT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
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
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | CST _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | IDENT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
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
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | CST _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | IDENT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
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
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | CST _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | IDENT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
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
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | CST _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | IDENT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
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
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | CST _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | IDENT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51

and _menhir_run53 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | CST _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | IDENT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53

and _menhir_goto_instruction : _menhir_env -> 'ttv_tail -> _menhir_state -> (Minic_ast.instr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IF ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | RETURN ->
        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | WHILE ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | END ->
        _menhir_reduce30 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68

and _menhir_goto_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | BEGIN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IF ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | RETURN ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | WHILE ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | END ->
                _menhir_reduce30 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55)
        | BNEQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | BEGIN | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 109 "minic_parser.mly"
                                 (Xor(a,b))
# 531 "minic_parser.ml"
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
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | BEGIN | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 118 "minic_parser.mly"
                                 (Sub(a,b))
# 588 "minic_parser.ml"
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
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | BEGIN | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 107 "minic_parser.mly"
                                (Or(a,b))
# 645 "minic_parser.ml"
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
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | BEGIN | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 111 "minic_parser.mly"
                                 (Neq(a,b))
# 702 "minic_parser.ml"
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
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | BEGIN | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 115 "minic_parser.mly"
                                 (Mul(a,b))
# 759 "minic_parser.ml"
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
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | BEGIN | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 117 "minic_parser.mly"
                                 (Mod(a,b))
# 816 "minic_parser.ml"
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
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | BEGIN | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 99 "minic_parser.mly"
                                ( Lt(a,b) )
# 873 "minic_parser.ml"
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
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | BEGIN | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 101 "minic_parser.mly"
                                 (Leqt(a,b))
# 930 "minic_parser.ml"
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
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | BEGIN | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 100 "minic_parser.mly"
                                (Gt(a,b))
# 987 "minic_parser.ml"
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
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | BEGIN | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 102 "minic_parser.mly"
                                 (Geqt(a,b))
# 1044 "minic_parser.ml"
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
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | BEGIN | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 103 "minic_parser.mly"
                                (Eq(a,b))
# 1101 "minic_parser.ml"
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
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | BEGIN | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 116 "minic_parser.mly"
                                 (Div(a,b))
# 1158 "minic_parser.ml"
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
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | BEGIN | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 110 "minic_parser.mly"
                                  (BXor(a,b))
# 1215 "minic_parser.ml"
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
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | BEGIN | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 108 "minic_parser.mly"
                                 (BOr(a,b))
# 1272 "minic_parser.ml"
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
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | BEGIN | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 112 "minic_parser.mly"
                                  (BNeq(a,b))
# 1329 "minic_parser.ml"
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
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | BEGIN | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 106 "minic_parser.mly"
                                  (BAnd(a,b))
# 1386 "minic_parser.ml"
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
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | BEGIN | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 105 "minic_parser.mly"
                                 (And(a,b))
# 1443 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | BEGIN | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 114 "minic_parser.mly"
                                 (Add(a,b))
# 1500 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Minic_ast.expr))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Minic_ast.instr) = 
# 85 "minic_parser.mly"
                           ( Return(e) )
# 1556 "minic_parser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | SUB ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | BEGIN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IF ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState61
            | RETURN ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState61
            | WHILE ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState61
            | END ->
                _menhir_reduce30 _menhir_env (Obj.magic _menhir_stack) MenhirState61
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61)
        | BNEQ ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState61 ->
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
                    | IF ->
                        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState65
                    | RETURN ->
                        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState65
                    | WHILE ->
                        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState65
                    | END ->
                        _menhir_reduce30 _menhir_env (Obj.magic _menhir_stack) MenhirState65
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | END | IF | RETURN | WHILE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((_menhir_stack, _menhir_s), _, (c : (Minic_ast.expr))), _, (e : (Minic_ast.seq))) = _menhir_stack in
                let _5 = () in
                let _3 = () in
                let _1 = () in
                let _v : (Minic_ast.instr) = 
# 87 "minic_parser.mly"
                                                  ( If(c,e,[]) )
# 1687 "minic_parser.ml"
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
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (c : (Minic_ast.expr))), _, (e1 : (Minic_ast.seq))), _, (e2 : (Minic_ast.seq))) = _menhir_stack in
            let _9 = () in
            let _7 = () in
            let _6 = () in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Minic_ast.instr) = 
# 86 "minic_parser.mly"
                                                                                         ( If(c,e1,e2) )
# 1721 "minic_parser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Minic_ast.instr))), _, (xs : (Minic_ast.seq))) = _menhir_stack in
        let _v : (Minic_ast.seq) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 1737 "minic_parser.ml"
         in
        _menhir_goto_list_instruction_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (c : (Minic_ast.expr))), _, (e : (Minic_ast.seq))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Minic_ast.instr) = 
# 88 "minic_parser.mly"
                                                     ( While(c,e) )
# 1756 "minic_parser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (t : (Minic_ast.typ))), (f : (
# 10 "minic_parser.mly"
       (string)
# 1777 "minic_parser.ml"
            ))), _, (s : (Minic_ast.seq))) = _menhir_stack in
            let _7 = () in
            let _5 = () in
            let _4 = () in
            let _3 = () in
            let _v : (Minic_ast.fun_def) = 
# 70 "minic_parser.mly"
   ( { 
       name = f; 
       code = s; 
       params = []; 
       return = t; 
       locals = [] 
   } )
# 1792 "minic_parser.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState74
            | INT ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState74
            | EOF ->
                _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState74
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run15 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 10 "minic_parser.mly"
       (string)
# 1821 "minic_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (i : (
# 10 "minic_parser.mly"
       (string)
# 1829 "minic_parser.ml"
    )) = _v in
    let _v : (Minic_ast.expr) = 
# 97 "minic_parser.mly"
          ( Get(i) )
# 1834 "minic_parser.ml"
     in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run16 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 8 "minic_parser.mly"
       (int)
# 1841 "minic_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (n : (
# 8 "minic_parser.mly"
       (int)
# 1849 "minic_parser.ml"
    )) = _v in
    let _v : (Minic_ast.expr) = 
# 95 "minic_parser.mly"
        ( Cst(n) )
# 1854 "minic_parser.ml"
     in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run17 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 9 "minic_parser.mly"
       (bool)
# 1861 "minic_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (b : (
# 9 "minic_parser.mly"
       (bool)
# 1869 "minic_parser.ml"
    )) = _v in
    let _v : (Minic_ast.expr) = 
# 96 "minic_parser.mly"
             ( BCst(b) )
# 1874 "minic_parser.ml"
     in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_variable_decl : _menhir_env -> 'ttv_tail -> _menhir_state -> (string * Minic_ast.typ * int) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | INT ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | EOF ->
        _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState4

and _menhir_reduce30 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Minic_ast.seq) = 
# 211 "<standard.mly>"
    ( [] )
# 1906 "minic_parser.ml"
     in
    _menhir_goto_list_instruction_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run14 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | CST _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | IDENT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14

and _menhir_run56 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | CST _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | IDENT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56

and _menhir_run59 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | CST _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | IDENT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59

and _menhir_goto_program : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 27 "minic_parser.mly"
      (Minic_ast.prog)
# 1964 "minic_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 27 "minic_parser.mly"
      (Minic_ast.prog)
# 1972 "minic_parser.ml"
    )) = _v in
    Obj.magic _1

and _menhir_goto_declaration_list : _menhir_env -> 'ttv_tail -> _menhir_state -> ((string * Minic_ast.typ * int) list * Minic_ast.fun_def list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (fd : (Minic_ast.fun_def))), _, (dl : ((string * Minic_ast.typ * int) list * Minic_ast.fun_def list))) = _menhir_stack in
        let _v : ((string * Minic_ast.typ * int) list * Minic_ast.fun_def list) = 
# 44 "minic_parser.mly"
                                           ( let vl, fl = dl in vl, fd :: fl )
# 1987 "minic_parser.ml"
         in
        _menhir_goto_declaration_list _menhir_env _menhir_stack _menhir_s _v
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (vd : (string * Minic_ast.typ * int))), _, (dl : ((string * Minic_ast.typ * int) list * Minic_ast.fun_def list))) = _menhir_stack in
        let _v : ((string * Minic_ast.typ * int) list * Minic_ast.fun_def list) = 
# 43 "minic_parser.mly"
                                           ( let vl, fl = dl in vd :: vl, fl )
# 1997 "minic_parser.ml"
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
            let (_menhir_stack, _menhir_s, (dl : ((string * Minic_ast.typ * int) list * Minic_ast.fun_def list))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 27 "minic_parser.mly"
      (Minic_ast.prog)
# 2013 "minic_parser.ml"
            ) = 
# 32 "minic_parser.mly"
                            ( let var_list, fun_list = dl in { globals = var_list; functions = fun_list; } )
# 2017 "minic_parser.ml"
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

and _menhir_goto_typ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Minic_ast.typ) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
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
                    | IF ->
                        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState13
                    | RETURN ->
                        _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState13
                    | WHILE ->
                        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState13
                    | END ->
                        _menhir_reduce30 _menhir_env (Obj.magic _menhir_stack) MenhirState13
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState13)
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
                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (t : (Minic_ast.typ))), (x : (
# 10 "minic_parser.mly"
       (string)
# 2088 "minic_parser.ml"
            ))) = _menhir_stack in
            let _3 = () in
            let _v : (string * Minic_ast.typ * int) = 
# 53 "minic_parser.mly"
                         ( (x, t, 0) )
# 2094 "minic_parser.ml"
             in
            _menhir_goto_variable_decl _menhir_env _menhir_stack _menhir_s _v
        | SET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CST _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | SEMI ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (((_menhir_stack, _menhir_s, (t : (Minic_ast.typ))), (x : (
# 10 "minic_parser.mly"
       (string)
# 2115 "minic_parser.ml"
                    ))), (n : (
# 8 "minic_parser.mly"
       (int)
# 2119 "minic_parser.ml"
                    ))) = _menhir_stack in
                    let _5 = () in
                    let _3 = () in
                    let _v : (string * Minic_ast.typ * int) = 
# 52 "minic_parser.mly"
                                     ( (x, t, n) )
# 2126 "minic_parser.ml"
                     in
                    _menhir_goto_variable_decl _menhir_env _menhir_stack _menhir_s _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
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
            let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
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
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState4 ->
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
# 27 "minic_parser.mly"
      (Minic_ast.prog)
# 2280 "minic_parser.ml"
        ) = let _startpos = _startpos__1_ in
        
# 34 "minic_parser.mly"
( 
   let pos = _startpos in
          let message = Printf.sprintf "Syntax error at %d, %d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol) in 
          failwith message 
)
# 2289 "minic_parser.ml"
         in
        _menhir_goto_program _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((string * Minic_ast.typ * int) list * Minic_ast.fun_def list) = 
# 42 "minic_parser.mly"
  ( [], [] )
# 2298 "minic_parser.ml"
     in
    _menhir_goto_declaration_list _menhir_env _menhir_stack _menhir_s _v

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Minic_ast.typ) = 
# 61 "minic_parser.mly"
      ( Int )
# 2310 "minic_parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Minic_ast.typ) = 
# 62 "minic_parser.mly"
       ( Bool )
# 2322 "minic_parser.ml"
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
# 27 "minic_parser.mly"
      (Minic_ast.prog)
# 2341 "minic_parser.ml"
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
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | INT ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | EOF ->
        _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

# 269 "<standard.mly>"
  

# 2369 "minic_parser.ml"
