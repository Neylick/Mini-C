
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
    | BXOR
    | BOR
    | BOOL_CST of (
# 9 "minic_parser.mly"
       (bool)
# 48 "minic_parser.ml"
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
  | MenhirState110
  | MenhirState107
  | MenhirState103
  | MenhirState94
  | MenhirState91
  | MenhirState88
  | MenhirState80
  | MenhirState78
  | MenhirState76
  | MenhirState69
  | MenhirState67
  | MenhirState66
  | MenhirState62
  | MenhirState60
  | MenhirState58
  | MenhirState54
  | MenhirState51
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
  | MenhirState15
  | MenhirState13
  | MenhirState11
  | MenhirState6
  | MenhirState5
  | MenhirState0

# 1 "minic_parser.mly"
  
  open Lexing
  open Minic_ast

# 116 "minic_parser.ml"

let rec _menhir_goto_function_decl : _menhir_env -> 'ttv_tail -> _menhir_state -> (Minic_ast.fun_def) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | IDENT _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v
    | INT ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | VOID ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | EOF ->
        _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState110
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState110

and _menhir_run11 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
    | CST _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
    | IDENT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
    | LPAR ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState11

and _menhir_run13 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
    | CST _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
    | IDENT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
    | LPAR ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState13

and _menhir_run15 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | CST _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | IDENT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | LPAR ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState15

and _menhir_run17 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
    | CST _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
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
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | CST _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
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
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
    | CST _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
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
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | CST _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
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
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | CST _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
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
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | CST _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
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
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | CST _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
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
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | CST _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
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
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | CST _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
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
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | CST _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
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
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | CST _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
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
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | CST _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
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
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | CST _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
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
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | CST _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
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
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | CST _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | IDENT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | LPAR ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45

and _menhir_goto_parameter_list : _menhir_env -> 'ttv_tail -> _menhir_state -> ((Minic_ast.typ * string) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState103 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (d : (Minic_ast.typ * string * Minic_ast.expr))), _, (p : ((Minic_ast.typ * string) list))) = _menhir_stack in
        let _2 = () in
        let _v : ((Minic_ast.typ * string) list) = 
# 65 "minic_parser.mly"
                                                 ( let (t, i, v) = d in (t,i)::p )
# 476 "minic_parser.ml"
         in
        _menhir_goto_parameter_list _menhir_env _menhir_stack _menhir_s _v
    | MenhirState58 ->
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
                | BOOL ->
                    _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState107
                | IDENT _v ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
                | IF ->
                    _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState107
                | INT ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState107
                | RETURN ->
                    _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState107
                | VOID ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState107
                | WHILE ->
                    _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState107
                | END ->
                    _menhir_reduce39 _menhir_env (Obj.magic _menhir_stack) MenhirState107
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState107)
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

and _menhir_goto_variable_decl : _menhir_env -> 'ttv_tail -> _menhir_state -> (Minic_ast.typ * string * Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState110 | MenhirState51 | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BOOL ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | IDENT _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
        | INT ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | VOID ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | EOF ->
            _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51)
    | MenhirState107 | MenhirState60 | MenhirState66 | MenhirState94 | MenhirState78 | MenhirState91 | MenhirState88 | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (decl : (Minic_ast.typ * string * Minic_ast.expr))) = _menhir_stack in
        let _v : (Minic_ast.instr) = 
# 107 "minic_parser.mly"
                           ( Set(decl) )
# 559 "minic_parser.ml"
         in
        _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_instruction_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Minic_ast.seq) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState80 ->
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
                    | BOOL ->
                        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState88
                    | IDENT _v ->
                        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
                    | IF ->
                        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState88
                    | INT ->
                        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState88
                    | RETURN ->
                        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState88
                    | VOID ->
                        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState88
                    | WHILE ->
                        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState88
                    | END ->
                        _menhir_reduce39 _menhir_env (Obj.magic _menhir_stack) MenhirState88
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState88)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | BOOL | END | IDENT _ | IF | INT | RETURN | VOID | WHILE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((((_menhir_stack, _menhir_s), _, (c : (Minic_ast.expr))), _), _, (e : (Minic_ast.seq))) = _menhir_stack in
                let _7 = () in
                let _5 = () in
                let _4 = () in
                let _2 = () in
                let _1 = () in
                let _v : (Minic_ast.instr) = 
# 99 "minic_parser.mly"
                                                                ( If(c, e, []) )
# 626 "minic_parser.ml"
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
    | MenhirState88 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s), _, (c : (Minic_ast.expr))), _), _, (e1 : (Minic_ast.seq))), _, (e2 : (Minic_ast.seq))) = _menhir_stack in
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
                                                                                                       ( If(c,e1,e2) )
# 662 "minic_parser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Minic_ast.instr))), _, (xs : (Minic_ast.seq))) = _menhir_stack in
        let _v : (Minic_ast.seq) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 678 "minic_parser.ml"
         in
        _menhir_goto_list_instruction_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (c : (Minic_ast.expr))), _, (e : (Minic_ast.seq))) = _menhir_stack in
            let _7 = () in
            let _5 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Minic_ast.instr) = 
# 104 "minic_parser.mly"
                                                                   ( While(c,e) )
# 699 "minic_parser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
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
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s, (t : (Minic_ast.typ))), (f : (
# 10 "minic_parser.mly"
       (string)
# 720 "minic_parser.ml"
            ))), _), _, (s : (Minic_ast.seq))) = _menhir_stack in
            let _7 = () in
            let _5 = () in
            let _4 = () in
            let _3 = () in
            let _v : (Minic_ast.fun_def) = 
# 77 "minic_parser.mly"
    ( { 
        name = f; 
        code = s; 
        params = []; 
        return = t;
    } )
# 734 "minic_parser.ml"
             in
            _menhir_goto_function_decl _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState107 ->
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
# 755 "minic_parser.ml"
            ))), _, (p : ((Minic_ast.typ * string) list))), _, (s : (Minic_ast.seq))) = _menhir_stack in
            let _8 = () in
            let _6 = () in
            let _5 = () in
            let _3 = () in
            let _v : (Minic_ast.fun_def) = 
# 85 "minic_parser.mly"
    ( { 
        name = f; 
        code = s; 
        params = p; 
        return = t;
    } )
# 769 "minic_parser.ml"
             in
            _menhir_goto_function_decl _menhir_env _menhir_stack _menhir_s _v
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
    | MenhirState107 | MenhirState60 | MenhirState66 | MenhirState80 | MenhirState91 | MenhirState88 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BOOL ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | IDENT _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
        | IF ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | INT ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | RETURN ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | VOID ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | WHILE ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | END ->
            _menhir_reduce39 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState91)
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ELSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | IDENT _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
            | IF ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | INT ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | RETURN ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | VOID ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | WHILE ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState94)
        | BOOL | END | IDENT _ | IF | INT | RETURN | VOID | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (c : (Minic_ast.expr))), _, (e : (Minic_ast.instr))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Minic_ast.instr) = 
# 101 "minic_parser.mly"
                                                 ( If(c, [e], []) )
# 847 "minic_parser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s), _, (c : (Minic_ast.expr))), _, (e1 : (Minic_ast.instr))), _, (e2 : (Minic_ast.instr))) = _menhir_stack in
        let _6 = () in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _v : (Minic_ast.instr) = 
# 100 "minic_parser.mly"
                                                                       ( If(c, [e1], [e2]) )
# 867 "minic_parser.ml"
         in
        _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

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
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Minic_ast.expr))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Minic_ast.expr) = 
# 111 "minic_parser.mly"
                             ( e )
# 924 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | SUB ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
        | END | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 126 "minic_parser.mly"
                                     (Xor(a,b))
# 985 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
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
        | ADD ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
        | END | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 135 "minic_parser.mly"
                                     (Sub(a,b))
# 1042 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
        | END | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 124 "minic_parser.mly"
                                    (Or(a,b))
# 1099 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
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
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
        | END | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 128 "minic_parser.mly"
                                     (Neq(a,b))
# 1156 "minic_parser.ml"
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
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
        | END | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 132 "minic_parser.mly"
                                     (Mul(a,b))
# 1213 "minic_parser.ml"
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
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
        | END | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 134 "minic_parser.mly"
                                     (Mod(a,b))
# 1270 "minic_parser.ml"
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
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
        | END | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 116 "minic_parser.mly"
                                    (Lt(a,b))
# 1327 "minic_parser.ml"
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
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
        | END | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 118 "minic_parser.mly"
                                     (Leqt(a,b))
# 1384 "minic_parser.ml"
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
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
        | END | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 117 "minic_parser.mly"
                                    (Gt(a,b))
# 1441 "minic_parser.ml"
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
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
        | END | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 119 "minic_parser.mly"
                                     (Geqt(a,b))
# 1498 "minic_parser.ml"
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
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
        | END | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 120 "minic_parser.mly"
                                    (Eq(a,b))
# 1555 "minic_parser.ml"
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
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
        | END | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 133 "minic_parser.mly"
                                     (Div(a,b))
# 1612 "minic_parser.ml"
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
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
        | END | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 127 "minic_parser.mly"
                                      (BXor(a,b))
# 1669 "minic_parser.ml"
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
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
        | END | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 125 "minic_parser.mly"
                                     (BOr(a,b))
# 1726 "minic_parser.ml"
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
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
        | END | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 129 "minic_parser.mly"
                                      (BNeq(a,b))
# 1783 "minic_parser.ml"
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
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
        | END | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 123 "minic_parser.mly"
                                      (BAnd(a,b))
# 1840 "minic_parser.ml"
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
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
        | END | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 122 "minic_parser.mly"
                                     (And(a,b))
# 1897 "minic_parser.ml"
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
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
        | END | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 131 "minic_parser.mly"
                                     (Add(a,b))
# 1954 "minic_parser.ml"
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
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (i : (
# 10 "minic_parser.mly"
       (string)
# 2007 "minic_parser.ml"
            ))), _, (v : (Minic_ast.expr))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (Minic_ast.typ * string * Minic_ast.expr) = 
# 71 "minic_parser.mly"
                                      ( (None, i, v) )
# 2014 "minic_parser.ml"
             in
            _menhir_goto_variable_decl _menhir_env _menhir_stack _menhir_s _v
        | SUB ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (t : (Minic_ast.typ))), (i : (
# 10 "minic_parser.mly"
       (string)
# 2071 "minic_parser.ml"
            ))), _, (v : (Minic_ast.expr))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _v : (Minic_ast.typ * string * Minic_ast.expr) = 
# 69 "minic_parser.mly"
                                              ( (t, i, v) )
# 2078 "minic_parser.ml"
             in
            _menhir_goto_variable_decl _menhir_env _menhir_stack _menhir_s _v
        | SUB ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
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
                | BOOL ->
                    _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | IDENT _v ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
                | IF ->
                    _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | INT ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | RETURN ->
                    _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | VOID ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | WHILE ->
                    _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | END ->
                    _menhir_reduce39 _menhir_env (Obj.magic _menhir_stack) MenhirState66
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66)
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
# 105 "minic_parser.mly"
                                        ( While(c, []) )
# 2170 "minic_parser.ml"
                 in
                _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | SUB ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
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
# 95 "minic_parser.mly"
                                         ( Return(e) )
# 2225 "minic_parser.ml"
                 in
                _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | EQ ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
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
# 2309 "minic_parser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | SUB ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState78 in
                let _menhir_stack = (_menhir_stack, _menhir_s) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BOOL ->
                    _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState80
                | IDENT _v ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
                | IF ->
                    _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState80
                | INT ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState80
                | RETURN ->
                    _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState80
                | VOID ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState80
                | WHILE ->
                    _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState80
                | END ->
                    _menhir_reduce39 _menhir_env (Obj.magic _menhir_stack) MenhirState80
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState80)
            | BOOL ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | IDENT _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
            | IF ->
                _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | INT ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | RETURN ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | SEMI ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState78 in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _, (c : (Minic_ast.expr))) = _menhir_stack in
                let _5 = () in
                let _4 = () in
                let _2 = () in
                let _1 = () in
                let _v : (Minic_ast.instr) = 
# 102 "minic_parser.mly"
                                     ( Skip )
# 2414 "minic_parser.ml"
                 in
                _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
            | VOID ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | WHILE ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState78)
        | SUB ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack)
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

and _menhir_run54 : _menhir_env -> ('ttv_tail * _menhir_state * (Minic_ast.typ)) * (
# 10 "minic_parser.mly"
       (string)
# 2446 "minic_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | CST _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | IDENT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | LPAR ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54

and _menhir_run57 : _menhir_env -> ('ttv_tail * _menhir_state * (Minic_ast.typ)) * (
# 10 "minic_parser.mly"
       (string)
# 2468 "minic_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let ((_menhir_stack, _menhir_s, (t : (Minic_ast.typ))), (i : (
# 10 "minic_parser.mly"
       (string)
# 2476 "minic_parser.ml"
    ))) = _menhir_stack in
    let _3 = () in
    let _v : (Minic_ast.typ * string * Minic_ast.expr) = 
# 60 "minic_parser.mly"
                           ( (t, i, Undef) )
# 2482 "minic_parser.ml"
     in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 | MenhirState110 | MenhirState51 | MenhirState107 | MenhirState60 | MenhirState66 | MenhirState94 | MenhirState78 | MenhirState91 | MenhirState88 | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (d : (Minic_ast.typ * string * Minic_ast.expr))) = _menhir_stack in
        let _v : (Minic_ast.typ * string * Minic_ast.expr) = 
# 70 "minic_parser.mly"
                        ( d )
# 2493 "minic_parser.ml"
         in
        _menhir_goto_variable_decl _menhir_env _menhir_stack _menhir_s _v
    | MenhirState103 | MenhirState58 ->
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
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState103
            | INT ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState103
            | VOID ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState103
            | RPAR ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState103 in
                let _v : ((Minic_ast.typ * string) list) = 
# 63 "minic_parser.mly"
    ( [] )
# 2518 "minic_parser.ml"
                 in
                _menhir_goto_parameter_list _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState103)
        | RPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (d : (Minic_ast.typ * string * Minic_ast.expr))) = _menhir_stack in
            let _v : ((Minic_ast.typ * string) list) = 
# 64 "minic_parser.mly"
                      ( let (t, i, v) = d in [(t,i)] )
# 2531 "minic_parser.ml"
             in
            _menhir_goto_parameter_list _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce39 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Minic_ast.seq) = 
# 211 "<standard.mly>"
    ( [] )
# 2548 "minic_parser.ml"
     in
    _menhir_goto_list_instruction_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run61 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
        | CST _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
        | IDENT _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
        | LPAR ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run67 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState67 in
        let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BOOL_CST _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
        | CST _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
        | IDENT _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
        | LPAR ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState69)
    | BOOL_CST _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | CST _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | IDENT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | LPAR ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | SEMI ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState67 in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : (Minic_ast.instr) = 
# 96 "minic_parser.mly"
                  ( Return(Undef) )
# 2626 "minic_parser.ml"
         in
        _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67

and _menhir_run75 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
        | CST _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
        | IDENT _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
        | LPAR ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState76)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_program : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 26 "minic_parser.mly"
      (Minic_ast.prog)
# 2667 "minic_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 26 "minic_parser.mly"
      (Minic_ast.prog)
# 2675 "minic_parser.ml"
    )) = _v in
    Obj.magic _1

and _menhir_goto_declaration_list : _menhir_env -> 'ttv_tail -> _menhir_state -> ((Minic_ast.typ * string * Minic_ast.expr) list * Minic_ast.fun_def list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState110 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (fd : (Minic_ast.fun_def))), _, (dl : ((Minic_ast.typ * string * Minic_ast.expr) list * Minic_ast.fun_def list))) = _menhir_stack in
        let _v : ((Minic_ast.typ * string * Minic_ast.expr) list * Minic_ast.fun_def list) = 
# 49 "minic_parser.mly"
                                             ( let vl, fl = dl in vl, fd :: fl )
# 2690 "minic_parser.ml"
         in
        _menhir_goto_declaration_list _menhir_env _menhir_stack _menhir_s _v
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (vd : (Minic_ast.typ * string * Minic_ast.expr))), _, (dl : ((Minic_ast.typ * string * Minic_ast.expr) list * Minic_ast.fun_def list))) = _menhir_stack in
        let _v : ((Minic_ast.typ * string * Minic_ast.expr) list * Minic_ast.fun_def list) = 
# 48 "minic_parser.mly"
                                             ( let vl, fl = dl in vd :: vl, fl )
# 2700 "minic_parser.ml"
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
# 26 "minic_parser.mly"
      (Minic_ast.prog)
# 2716 "minic_parser.ml"
            ) = 
# 32 "minic_parser.mly"
  ( let var_list, fun_list = dl in 
    { 
      globals = var_list; 
      functions = fun_list; 
    } 
  )
# 2725 "minic_parser.ml"
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
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | CST _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
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
# 2759 "minic_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (i : (
# 10 "minic_parser.mly"
       (string)
# 2767 "minic_parser.ml"
    )) = _v in
    let _v : (Minic_ast.expr) = 
# 114 "minic_parser.mly"
              ( Get(i) )
# 2772 "minic_parser.ml"
     in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 8 "minic_parser.mly"
       (int)
# 2779 "minic_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (n : (
# 8 "minic_parser.mly"
       (int)
# 2787 "minic_parser.ml"
    )) = _v in
    let _v : (Minic_ast.expr) = 
# 112 "minic_parser.mly"
            ( Cst(n) )
# 2792 "minic_parser.ml"
     in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 9 "minic_parser.mly"
       (bool)
# 2799 "minic_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (b : (
# 9 "minic_parser.mly"
       (bool)
# 2807 "minic_parser.ml"
    )) = _v in
    let _v : (Minic_ast.expr) = 
# 113 "minic_parser.mly"
                 ( BCst(b) )
# 2812 "minic_parser.ml"
     in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_typ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Minic_ast.typ) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 | MenhirState110 | MenhirState51 ->
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
                    _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState58
                | INT ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState58
                | RPAR ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_s = MenhirState58 in
                    let _menhir_stack = (_menhir_stack, _menhir_s) in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | BEGIN ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        (match _tok with
                        | BOOL ->
                            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState60
                        | IDENT _v ->
                            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
                        | IF ->
                            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState60
                        | INT ->
                            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState60
                        | RETURN ->
                            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState60
                        | VOID ->
                            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState60
                        | WHILE ->
                            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState60
                        | END ->
                            _menhir_reduce39 _menhir_env (Obj.magic _menhir_stack) MenhirState60
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (_menhir_stack, _menhir_s) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | VOID ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState58
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58)
            | SEMI ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
            | SET ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState107 | MenhirState60 | MenhirState66 | MenhirState94 | MenhirState78 | MenhirState91 | MenhirState88 | MenhirState80 ->
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
            | SEMI ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
            | SET ->
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState103 | MenhirState58 ->
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
            | SEMI ->
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState110 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState107 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState103 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState88 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState51 ->
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
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState11 ->
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
# 26 "minic_parser.mly"
      (Minic_ast.prog)
# 3117 "minic_parser.ml"
        ) = let _startpos = _startpos__1_ in
        
# 39 "minic_parser.mly"
  ( 
    let pos = _startpos in
    let message = Printf.sprintf "Syntax error at %d, %d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol) in 
    failwith message 
  )
# 3126 "minic_parser.ml"
         in
        _menhir_goto_program _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((Minic_ast.typ * string * Minic_ast.expr) list * Minic_ast.fun_def list) = 
# 47 "minic_parser.mly"
    ( [], [] )
# 3135 "minic_parser.ml"
     in
    _menhir_goto_declaration_list _menhir_env _menhir_stack _menhir_s _v

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Minic_ast.typ) = 
# 56 "minic_parser.mly"
         ( Void )
# 3147 "minic_parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Minic_ast.typ) = 
# 54 "minic_parser.mly"
        ( Int )
# 3159 "minic_parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 10 "minic_parser.mly"
       (string)
# 3166 "minic_parser.ml"
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
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
        | CST _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
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

and _menhir_run50 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Minic_ast.typ) = 
# 55 "minic_parser.mly"
         ( Bool )
# 3205 "minic_parser.ml"
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
# 26 "minic_parser.mly"
      (Minic_ast.prog)
# 3224 "minic_parser.ml"
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
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | IDENT _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | INT ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | VOID ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | EOF ->
        _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

# 269 "<standard.mly>"
  

# 3256 "minic_parser.ml"
