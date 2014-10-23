type token =
  | IDENT of (string)
  | PREFIX of (string)
  | INFIX of (string)
  | INT of (int)
  | FLOAT of (float)
  | STRING of (string)
  | EOF
  | EQUAL
  | LPAREN
  | RPAREN
  | COMMA
  | MINUSGREATER
  | COLONCOLON
  | COLONEQUAL
  | SEMI
  | SEMISEMI
  | LBRACKET
  | RBRACKET
  | ELSE
  | FUNCTION
  | IF
  | IN
  | LET
  | REC
  | REF
  | THEN

open Parsing;;
let _ = parse_error;;
# 2 "asyn.mly"
open Util;;
open Types;;
let make_apply e l = it_list (function a -> function b -> App (a,b)) e l;;
# 36 "asyn.ml"
let yytransl_const = [|
    0 (* EOF *);
  263 (* EQUAL *);
  264 (* LPAREN *);
  265 (* RPAREN *);
  266 (* COMMA *);
  267 (* MINUSGREATER *);
  268 (* COLONCOLON *);
  269 (* COLONEQUAL *);
  270 (* SEMI *);
  271 (* SEMISEMI *);
  272 (* LBRACKET *);
  273 (* RBRACKET *);
  274 (* ELSE *);
  275 (* FUNCTION *);
  276 (* IF *);
  277 (* IN *);
  278 (* LET *);
  279 (* REC *);
  280 (* REF *);
  281 (* THEN *);
    0|]

let yytransl_block = [|
  257 (* IDENT *);
  258 (* PREFIX *);
  259 (* INFIX *);
  260 (* INT *);
  261 (* FLOAT *);
  262 (* STRING *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\001\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\003\000\003\000\003\000\003\000\003\000\003\000\004\000\
\004\000\005\000\005\000\005\000\000\000"

let yylen = "\002\000\
\002\000\005\000\006\000\001\000\001\000\002\000\002\000\003\000\
\003\000\003\000\003\000\003\000\006\000\004\000\006\000\007\000\
\004\000\001\000\001\000\002\000\002\000\003\000\002\000\002\000\
\001\000\001\000\001\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\019\000\000\000\026\000\027\000\028\000\004\000\
\000\000\000\000\000\000\000\000\000\000\000\000\029\000\000\000\
\000\000\018\000\023\000\020\000\000\000\000\000\021\000\000\000\
\000\000\000\000\000\000\007\000\001\000\000\000\000\000\000\000\
\000\000\000\000\000\000\006\000\000\000\000\000\022\000\000\000\
\000\000\000\000\000\000\011\000\012\000\008\000\009\000\010\000\
\024\000\000\000\000\000\017\000\000\000\000\000\000\000\000\000\
\000\000\000\000\002\000\000\000\000\000\000\000\013\000\015\000\
\003\000\000\000\016\000"

let yydgoto = "\002\000\
\015\000\016\000\017\000\036\000\018\000"

let yysindex = "\011\000\
\001\000\000\000\000\000\104\255\000\000\000\000\000\000\000\000\
\034\255\252\254\016\255\058\255\003\255\058\255\000\000\005\255\
\091\255\000\000\000\000\000\000\006\255\018\255\000\000\017\255\
\009\255\023\255\043\255\000\000\000\000\058\255\058\255\058\255\
\058\255\058\255\104\255\000\000\038\255\051\255\000\000\058\255\
\058\255\058\255\040\255\000\000\000\000\000\000\000\000\000\000\
\000\000\058\255\061\255\000\000\047\255\250\254\058\255\048\255\
\058\255\058\255\000\000\058\255\251\254\049\255\000\000\000\000\
\000\000\058\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\249\254\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\066\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\046\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\247\255\002\000\037\000\000\000"

let yytablesize = 281
let yytable = "\022\000\
\008\000\005\000\025\000\026\000\028\000\019\000\037\000\005\000\
\059\000\065\000\005\000\001\000\023\000\005\000\060\000\066\000\
\024\000\005\000\035\000\029\000\044\000\045\000\046\000\047\000\
\048\000\027\000\039\000\040\000\038\000\042\000\052\000\053\000\
\054\000\041\000\003\000\004\000\035\000\005\000\006\000\007\000\
\056\000\009\000\020\000\043\000\050\000\061\000\055\000\062\000\
\063\000\010\000\064\000\051\000\011\000\012\000\014\000\021\000\
\067\000\014\000\003\000\004\000\014\000\005\000\006\000\007\000\
\058\000\009\000\014\000\057\000\060\000\066\000\014\000\049\000\
\000\000\010\000\025\000\000\000\011\000\012\000\000\000\021\000\
\025\000\014\000\000\000\025\000\000\000\000\000\025\000\000\000\
\000\000\000\000\025\000\003\000\004\000\030\000\005\000\006\000\
\007\000\031\000\009\000\000\000\032\000\000\000\033\000\034\000\
\003\000\004\000\010\000\005\000\006\000\007\000\000\000\009\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\010\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\003\000\004\000\000\000\005\000\006\000\007\000\000\000\
\009\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\010\000\000\000\000\000\011\000\012\000\000\000\013\000\000\000\
\014\000"

let yycheck = "\009\000\
\000\000\009\001\012\000\001\001\014\000\004\000\001\001\015\001\
\015\001\015\001\018\001\001\000\017\001\021\001\021\001\021\001\
\001\001\025\001\017\000\015\001\030\000\031\000\032\000\033\000\
\034\000\023\001\009\001\011\001\023\001\007\001\040\000\041\000\
\042\000\025\001\001\001\002\001\035\000\004\001\005\001\006\001\
\050\000\008\001\009\001\001\001\007\001\055\000\007\001\057\000\
\058\000\016\001\060\000\001\001\019\001\020\001\009\001\022\001\
\066\000\024\001\001\001\002\001\015\001\004\001\005\001\006\001\
\018\001\008\001\021\001\007\001\021\001\021\001\025\001\035\000\
\255\255\016\001\009\001\255\255\019\001\020\001\255\255\022\001\
\015\001\024\001\255\255\018\001\255\255\255\255\021\001\255\255\
\255\255\255\255\025\001\001\001\002\001\003\001\004\001\005\001\
\006\001\007\001\008\001\255\255\010\001\255\255\012\001\013\001\
\001\001\002\001\016\001\004\001\005\001\006\001\255\255\008\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\016\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\001\001\002\001\255\255\004\001\005\001\006\001\255\255\
\008\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\016\001\255\255\255\255\019\001\020\001\255\255\022\001\255\255\
\024\001"

let yynames_const = "\
  EOF\000\
  EQUAL\000\
  LPAREN\000\
  RPAREN\000\
  COMMA\000\
  MINUSGREATER\000\
  COLONCOLON\000\
  COLONEQUAL\000\
  SEMI\000\
  SEMISEMI\000\
  LBRACKET\000\
  RBRACKET\000\
  ELSE\000\
  FUNCTION\000\
  IF\000\
  IN\000\
  LET\000\
  REC\000\
  REF\000\
  THEN\000\
  "

let yynames_block = "\
  IDENT\000\
  PREFIX\000\
  INFIX\000\
  INT\000\
  FLOAT\000\
  STRING\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'Expr) in
    Obj.repr(
# 62 "asyn.mly"
          ( Expr _1 )
# 236 "asyn.ml"
               : Types.ml_phrase))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'Expr) in
    Obj.repr(
# 64 "asyn.mly"
   ( Decl (Let(false,_2,_4)))
# 244 "asyn.ml"
               : Types.ml_phrase))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'Expr) in
    Obj.repr(
# 66 "asyn.mly"
   ( Decl (Let(true,_3,_5)))
# 252 "asyn.ml"
               : Types.ml_phrase))
; (fun __caml_parser_env ->
    Obj.repr(
# 68 "asyn.mly"
          ( raise End_of_file )
# 258 "asyn.ml"
               : Types.ml_phrase))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Simple_expr) in
    Obj.repr(
# 76 "asyn.mly"
          ( _1 )
# 265 "asyn.ml"
               : 'Expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'Simple_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Simple_expr_list) in
    Obj.repr(
# 78 "asyn.mly"
   (make_apply _1 _2)
# 273 "asyn.ml"
               : 'Expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Expr) in
    Obj.repr(
# 80 "asyn.mly"
   (Ref _2)
# 280 "asyn.ml"
               : 'Expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Simple_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Expr) in
    Obj.repr(
# 82 "asyn.mly"
   (Pair (_1,_3))
# 288 "asyn.ml"
               : 'Expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Simple_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Expr) in
    Obj.repr(
# 84 "asyn.mly"
   (Cons (_1,_3))
# 296 "asyn.ml"
               : 'Expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Simple_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Expr) in
    Obj.repr(
# 86 "asyn.mly"
   (Binop (":=",_1,_3))
# 304 "asyn.ml"
               : 'Expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Simple_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Expr) in
    Obj.repr(
# 88 "asyn.mly"
   ( Binop (_2, _1,_3))
# 313 "asyn.ml"
               : 'Expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Simple_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'Expr) in
    Obj.repr(
# 90 "asyn.mly"
          ( Binop ("=",_1,_3))
# 321 "asyn.ml"
               : 'Expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'Expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Expr) in
    Obj.repr(
# 92 "asyn.mly"
          ( Cond(_2, _4, _6) )
# 330 "asyn.ml"
               : 'Expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'Expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'Expr) in
    Obj.repr(
# 94 "asyn.mly"
          ( Cond(_2, _4, Const Unit))
# 338 "asyn.ml"
               : 'Expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Expr) in
    Obj.repr(
# 96 "asyn.mly"
          ( Letin (false,_2,_4,_6) )
# 347 "asyn.ml"
               : 'Expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'Expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'Expr) in
    Obj.repr(
# 98 "asyn.mly"
          ( Letin (true,_3,_5,_7) )
# 356 "asyn.ml"
               : 'Expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'Expr) in
    Obj.repr(
# 100 "asyn.mly"
          ( Abs(_2,_4) )
# 364 "asyn.ml"
               : 'Expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Struct_constant) in
    Obj.repr(
# 104 "asyn.mly"
          ( Const _1 )
# 371 "asyn.ml"
               : 'Simple_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 106 "asyn.mly"
   (Var _1)
# 378 "asyn.ml"
               : 'Simple_expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 108 "asyn.mly"
          ( Const  Unit )
# 384 "asyn.ml"
               : 'Simple_expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 110 "asyn.mly"
         ( Const Emptylist)
# 390 "asyn.ml"
               : 'Simple_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Expr) in
    Obj.repr(
# 112 "asyn.mly"
          ( _2 )
# 397 "asyn.ml"
               : 'Simple_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Simple_expr) in
    Obj.repr(
# 114 "asyn.mly"
          ( Unop ( _1,_2))
# 405 "asyn.ml"
               : 'Simple_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'Simple_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'Simple_expr_list) in
    Obj.repr(
# 120 "asyn.mly"
          ( _1 :: _2 )
# 413 "asyn.ml"
               : 'Simple_expr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Simple_expr) in
    Obj.repr(
# 122 "asyn.mly"
          ( [_1] )
# 420 "asyn.ml"
               : 'Simple_expr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 126 "asyn.mly"
          ( Int _1 )
# 427 "asyn.ml"
               : 'Struct_constant))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 128 "asyn.mly"
          ( Float _1 )
# 434 "asyn.ml"
               : 'Struct_constant))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 130 "asyn.mly"
          ( String  _1 )
# 441 "asyn.ml"
               : 'Struct_constant))
(* Entry implementation *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let implementation (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Types.ml_phrase)
;;
# 135 "asyn.mly"



# 470 "asyn.ml"
