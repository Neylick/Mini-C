
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | XOR
    | WHILE
    | VOID
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
# 28 "minic_parser.ml"
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
# 40 "minic_parser.ml"
  )
    | BXOR
    | BOR
    | BOOL_CST of (
# 9 "minic_parser.mly"
       (bool)
# 47 "minic_parser.ml"
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
  | MenhirState92
  | MenhirState86
  | MenhirState83
  | MenhirState76
  | MenhirState73
  | MenhirState66
  | MenhirState64
  | MenhirState63
  | MenhirState59
  | MenhirState57
  | MenhirState51
  | MenhirState48
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
  | MenhirState10
  | MenhirState5
  | MenhirState0

# 1 "minic_parser.mly"
  
  open Lexing
  open Minic_ast

# 109 "minic_parser.ml"

let rec _menhir_run10 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
    | CST _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
    | IDENT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState10

and _menhir_run12 : _menhir_env -> 'ttv_tail * _menhir_state * (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | CST _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | IDENT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
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
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | CST _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | IDENT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
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
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | CST _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | IDENT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
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
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | CST _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | IDENT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
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
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | CST _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | IDENT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
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
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
    | CST _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
    | IDENT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
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
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
    | CST _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
    | IDENT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
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
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | CST _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | IDENT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
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
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
    | CST _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
    | IDENT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
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
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | CST _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | IDENT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
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
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | CST _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | IDENT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
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
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | CST _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | IDENT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
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
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | CST _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | IDENT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
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
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | CST _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | IDENT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
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
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | CST _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | IDENT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
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
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | CST _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | IDENT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
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
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | CST _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | IDENT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState44

and _menhir_goto_variable_decl : _menhir_env -> 'ttv_tail -> _menhir_state -> (Minic_ast.typ * string * Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState92 | MenhirState48 | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BOOL ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | IDENT _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
        | INT ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | VOID ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | EOF ->
            _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48)
    | MenhirState57 | MenhirState63 | MenhirState86 | MenhirState83 | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (decl : (Minic_ast.typ * string * Minic_ast.expr))) = _menhir_stack in
        let _v : (Minic_ast.instr) = 
# 90 "minic_parser.mly"
                       ( Set(decl) )
# 429 "minic_parser.ml"
         in
        _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_instruction_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Minic_ast.seq) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState76 ->
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
                        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState83
                    | IDENT _v ->
                        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
                    | IF ->
                        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState83
                    | INT ->
                        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState83
                    | RETURN ->
                        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState83
                    | VOID ->
                        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState83
                    | WHILE ->
                        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState83
                    | END ->
                        _menhir_reduce34 _menhir_env (Obj.magic _menhir_stack) MenhirState83
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
            | BOOL | END | IDENT _ | IF | INT | RETURN | VOID | WHILE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((_menhir_stack, _menhir_s), _, (c : (Minic_ast.expr))), _, (e : (Minic_ast.seq))) = _menhir_stack in
                let _7 = () in
                let _5 = () in
                let _4 = () in
                let _2 = () in
                let _1 = () in
                let _v : (Minic_ast.instr) = 
# 85 "minic_parser.mly"
                                                              ( If(c, e, []) )
# 496 "minic_parser.ml"
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
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (c : (Minic_ast.expr))), _, (e1 : (Minic_ast.seq))), _, (e2 : (Minic_ast.seq))) = _menhir_stack in
            let _11 = () in
            let _9 = () in
            let _8 = () in
            let _7 = () in
            let _5 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Minic_ast.instr) = 
# 84 "minic_parser.mly"
                                                                                                     ( If(c,e1,e2) )
# 532 "minic_parser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState86 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Minic_ast.instr))), _, (xs : (Minic_ast.seq))) = _menhir_stack in
        let _v : (Minic_ast.seq) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 548 "minic_parser.ml"
         in
        _menhir_goto_list_instruction_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState63 ->
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
# 87 "minic_parser.mly"
                                                                 ( While(c,e) )
# 569 "minic_parser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState57 ->
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
# 590 "minic_parser.ml"
            ))), _, (s : (Minic_ast.seq))) = _menhir_stack in
            let _7 = () in
            let _5 = () in
            let _4 = () in
            let _3 = () in
            let _v : (Minic_ast.fun_def) = 
# 71 "minic_parser.mly"
    ( { 
        name = f; 
        code = s; 
        params = []; 
        return = t;
    } )
# 604 "minic_parser.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | IDENT _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
            | INT ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | VOID ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | EOF ->
                _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState92)
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
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | IDENT _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
    | IF ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | INT ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | RETURN ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | VOID ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | WHILE ->
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | END ->
        _menhir_reduce34 _menhir_env (Obj.magic _menhir_stack) MenhirState86
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState86

and _menhir_goto_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> (Minic_ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (i : (
# 10 "minic_parser.mly"
       (string)
# 710 "minic_parser.ml"
            ))), _, (v : (Minic_ast.expr))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (Minic_ast.typ * string * Minic_ast.expr) = 
# 63 "minic_parser.mly"
                                      ( (None, i, v) )
# 717 "minic_parser.ml"
             in
            _menhir_goto_variable_decl _menhir_env _menhir_stack _menhir_s _v
        | SUB ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack)
        | END | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 108 "minic_parser.mly"
                                   (Xor(a,b))
# 778 "minic_parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
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
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack)
        | END | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 117 "minic_parser.mly"
                                   (Sub(a,b))
# 835 "minic_parser.ml"
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
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack)
        | END | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 106 "minic_parser.mly"
                                  (Or(a,b))
# 892 "minic_parser.ml"
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
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack)
        | END | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 110 "minic_parser.mly"
                                   (Neq(a,b))
# 949 "minic_parser.ml"
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
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack)
        | END | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 114 "minic_parser.mly"
                                   (Mul(a,b))
# 1006 "minic_parser.ml"
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
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack)
        | END | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 116 "minic_parser.mly"
                                   (Mod(a,b))
# 1063 "minic_parser.ml"
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
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack)
        | END | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 98 "minic_parser.mly"
                                  (Lt(a,b))
# 1120 "minic_parser.ml"
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
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack)
        | END | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 100 "minic_parser.mly"
                                   (Leqt(a,b))
# 1177 "minic_parser.ml"
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
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack)
        | END | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 99 "minic_parser.mly"
                                  (Gt(a,b))
# 1234 "minic_parser.ml"
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
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack)
        | END | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 101 "minic_parser.mly"
                                   (Geqt(a,b))
# 1291 "minic_parser.ml"
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
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack)
        | END | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 102 "minic_parser.mly"
                                  (Eq(a,b))
# 1348 "minic_parser.ml"
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
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack)
        | END | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 115 "minic_parser.mly"
                                   (Div(a,b))
# 1405 "minic_parser.ml"
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
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack)
        | END | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 109 "minic_parser.mly"
                                    (BXor(a,b))
# 1462 "minic_parser.ml"
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
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack)
        | END | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 107 "minic_parser.mly"
                                   (BOr(a,b))
# 1519 "minic_parser.ml"
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
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack)
        | END | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 111 "minic_parser.mly"
                                    (BNeq(a,b))
# 1576 "minic_parser.ml"
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
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack)
        | END | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 105 "minic_parser.mly"
                                    (BAnd(a,b))
# 1633 "minic_parser.ml"
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
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack)
        | END | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 104 "minic_parser.mly"
                                   (And(a,b))
# 1690 "minic_parser.ml"
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
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack)
        | END | RPAR | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a : (Minic_ast.expr))), _, (b : (Minic_ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Minic_ast.expr) = 
# 113 "minic_parser.mly"
                                   (Add(a,b))
# 1747 "minic_parser.ml"
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
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (t : (Minic_ast.typ))), (i : (
# 10 "minic_parser.mly"
       (string)
# 1800 "minic_parser.ml"
            ))), _, (v : (Minic_ast.expr))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _v : (Minic_ast.typ * string * Minic_ast.expr) = 
# 61 "minic_parser.mly"
                                              ( (t, i, v) )
# 1807 "minic_parser.ml"
             in
            _menhir_goto_variable_decl _menhir_env _menhir_stack _menhir_s _v
        | SUB ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
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
                    _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState63
                | IDENT _v ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
                | IF ->
                    _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState63
                | INT ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState63
                | RETURN ->
                    _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState63
                | VOID ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState63
                | WHILE ->
                    _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState63
                | END ->
                    _menhir_reduce34 _menhir_env (Obj.magic _menhir_stack) MenhirState63
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63)
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
# 88 "minic_parser.mly"
                                      ( While(c, []) )
# 1899 "minic_parser.ml"
                 in
                _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | SUB ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
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
# 81 "minic_parser.mly"
                                       ( Return(e) )
# 1954 "minic_parser.ml"
                 in
                _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | EQ ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Minic_ast.expr))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Minic_ast.instr) = 
# 80 "minic_parser.mly"
                             ( Return(e) )
# 2038 "minic_parser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | SUB ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | BAND ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | BNEQ ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | BOR ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | BXOR ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | GET ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | LET ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
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
                    _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState76
                | IDENT _v ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
                | IF ->
                    _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState76
                | INT ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState76
                | RETURN ->
                    _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState76
                | VOID ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState76
                | WHILE ->
                    _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState76
                | END ->
                    _menhir_reduce34 _menhir_env (Obj.magic _menhir_stack) MenhirState76
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState76)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | SUB ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack)
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

and _menhir_run51 : _menhir_env -> ('ttv_tail * _menhir_state * (Minic_ast.typ)) * (
# 10 "minic_parser.mly"
       (string)
# 2145 "minic_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL_CST _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | CST _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | IDENT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51

and _menhir_run54 : _menhir_env -> ('ttv_tail * _menhir_state * (Minic_ast.typ)) * (
# 10 "minic_parser.mly"
       (string)
# 2165 "minic_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let ((_menhir_stack, _menhir_s, (t : (Minic_ast.typ))), (i : (
# 10 "minic_parser.mly"
       (string)
# 2173 "minic_parser.ml"
    ))) = _menhir_stack in
    let _3 = () in
    let _v : (Minic_ast.typ * string * Minic_ast.expr) = 
# 62 "minic_parser.mly"
                           ( (t, i, Undef) )
# 2179 "minic_parser.ml"
     in
    _menhir_goto_variable_decl _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce34 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Minic_ast.seq) = 
# 211 "<standard.mly>"
    ( [] )
# 2188 "minic_parser.ml"
     in
    _menhir_goto_list_instruction_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run58 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
        | CST _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
        | IDENT _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run64 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState64 in
        let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BOOL_CST _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
        | CST _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
        | IDENT _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66)
    | BOOL_CST _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
    | CST _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
    | IDENT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
    | SEMI ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState64 in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : (Minic_ast.instr) = 
# 82 "minic_parser.mly"
                ( Return(Undef) )
# 2260 "minic_parser.ml"
         in
        _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64

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
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
        | CST _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
        | IDENT _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
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

and _menhir_goto_program : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 26 "minic_parser.mly"
      (Minic_ast.prog)
# 2299 "minic_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 26 "minic_parser.mly"
      (Minic_ast.prog)
# 2307 "minic_parser.ml"
    )) = _v in
    Obj.magic _1

and _menhir_goto_declaration_list : _menhir_env -> 'ttv_tail -> _menhir_state -> ((Minic_ast.typ * string * Minic_ast.expr) list * Minic_ast.fun_def list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState92 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (fd : (Minic_ast.fun_def))), _, (dl : ((Minic_ast.typ * string * Minic_ast.expr) list * Minic_ast.fun_def list))) = _menhir_stack in
        let _v : ((Minic_ast.typ * string * Minic_ast.expr) list * Minic_ast.fun_def list) = 
# 49 "minic_parser.mly"
                                           ( let vl, fl = dl in vl, fd :: fl )
# 2322 "minic_parser.ml"
         in
        _menhir_goto_declaration_list _menhir_env _menhir_stack _menhir_s _v
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (vd : (Minic_ast.typ * string * Minic_ast.expr))), _, (dl : ((Minic_ast.typ * string * Minic_ast.expr) list * Minic_ast.fun_def list))) = _menhir_stack in
        let _v : ((Minic_ast.typ * string * Minic_ast.expr) list * Minic_ast.fun_def list) = 
# 48 "minic_parser.mly"
                                           ( let vl, fl = dl in vd :: vl, fl )
# 2332 "minic_parser.ml"
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
# 2348 "minic_parser.ml"
            ) = 
# 32 "minic_parser.mly"
  ( let var_list, fun_list = dl in 
    { 
      globals = var_list; 
      functions = fun_list; 
    } 
  )
# 2357 "minic_parser.ml"
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

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 10 "minic_parser.mly"
       (string)
# 2372 "minic_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (i : (
# 10 "minic_parser.mly"
       (string)
# 2380 "minic_parser.ml"
    )) = _v in
    let _v : (Minic_ast.expr) = 
# 96 "minic_parser.mly"
            ( Get(i) )
# 2385 "minic_parser.ml"
     in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 8 "minic_parser.mly"
       (int)
# 2392 "minic_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (n : (
# 8 "minic_parser.mly"
       (int)
# 2400 "minic_parser.ml"
    )) = _v in
    let _v : (Minic_ast.expr) = 
# 94 "minic_parser.mly"
          ( Cst(n) )
# 2405 "minic_parser.ml"
     in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 9 "minic_parser.mly"
       (bool)
# 2412 "minic_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (b : (
# 9 "minic_parser.mly"
       (bool)
# 2420 "minic_parser.ml"
    )) = _v in
    let _v : (Minic_ast.expr) = 
# 95 "minic_parser.mly"
               ( BCst(b) )
# 2425 "minic_parser.ml"
     in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_typ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Minic_ast.typ) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 | MenhirState92 | MenhirState48 ->
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
                            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState57
                        | IDENT _v ->
                            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
                        | IF ->
                            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState57
                        | INT ->
                            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState57
                        | RETURN ->
                            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState57
                        | VOID ->
                            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState57
                        | WHILE ->
                            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState57
                        | END ->
                            _menhir_reduce34 _menhir_env (Obj.magic _menhir_stack) MenhirState57
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57)
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
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
            | SET ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState57 | MenhirState63 | MenhirState86 | MenhirState83 | MenhirState76 ->
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
                _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
            | SET ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState92 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState86 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState48 ->
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
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
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
# 2675 "minic_parser.ml"
        ) = let _startpos = _startpos__1_ in
        
# 39 "minic_parser.mly"
  ( 
    let pos = _startpos in
    let message = Printf.sprintf "Syntax error at %d, %d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol) in 
    failwith message 
  )
# 2684 "minic_parser.ml"
         in
        _menhir_goto_program _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((Minic_ast.typ * string * Minic_ast.expr) list * Minic_ast.fun_def list) = 
# 47 "minic_parser.mly"
  ( [], [] )
# 2693 "minic_parser.ml"
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
# 2705 "minic_parser.ml"
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
# 2717 "minic_parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 10 "minic_parser.mly"
       (string)
# 2724 "minic_parser.ml"
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
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
        | CST _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
        | IDENT _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
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

and _menhir_run47 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Minic_ast.typ) = 
# 55 "minic_parser.mly"
         ( Bool )
# 2761 "minic_parser.ml"
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
# 2780 "minic_parser.ml"
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
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState0
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
  

# 2812 "minic_parser.ml"
