%{
open Util;;
open Types;;
let make_apply e l = it_list (function a -> function b -> App (a,b)) e l;;
%}

/* Tokens */

/* Identifiers */


%token <string> IDENT
%token <string> PREFIX
%token <string> INFIX


/* Literals */
%token <int> INT
%token <float> FLOAT
%token <string> STRING
/* The end-of-file marker */
%token EOF
/* Special symbols */
%token EQUAL          /* "=" */
%token LPAREN         /* "(" */
%token RPAREN         /* ")" */
%token COMMA          /* "," */
%token MINUSGREATER   /* "->" */
%token COLONCOLON     /* "::" */
%token COLONEQUAL     /* ":=" */
%token SEMI           /* ";" */
%token SEMISEMI       /* ";;" */
%token LBRACKET       /* "[" */
%token RBRACKET       /* "]" */
/* Keywords */
%token ELSE           /* "else" */
%token FUNCTION       /* "function" */
%token FUN            /* "function" */
%token IF             /* "if" */
%token IN             /* "in" */
%token LET            /* "let" */
%token REC            /* "rec" */
%token REF            /* "ref" */
%token THEN           /* "then" */



/* Precedences and associativities. Lower precedences first. */

%right prec_let
%right prec_if
%right prec_app
%right PREFIX

/* Entry points */

%start start
%type <Types.ml_phrase list> start

%%

start: 
 | implementation SEMISEMI start { $1::$3 }
 | implementation start { $1::$2 }
 | implementation SEMISEMI { [$1] }
 | implementation { [$1] }

 | EOF { [] }
;

implementation :
        Expr
          { Expr $1 }
      | LET IDENT EQUAL Expr 
	  { Decl (Let(false,$2,$4))}
      | LET REC IDENT EQUAL Expr
	  { Decl (Let(true,$3,$5))}
;


/* Expressions */

Expr :
        Simple_expr
          { $1 }
      | Simple_expr Simple_expr_list   %prec prec_app
	  {make_apply $1 $2}
      | REF Expr
	  {Ref $2}
      | Simple_expr COMMA Expr
	  {Pair ($1,$3)}
      | Simple_expr  COLONCOLON Expr
	  {Cons ($1,$3)}
      | Simple_expr  COLONEQUAL Expr
	  {Binop (":=",$1,$3)}
      | Simple_expr INFIX Expr 
	  { Binop ($2, $1,$3)}
      | Simple_expr EQUAL Expr
          { Binop ("=",$1,$3)}
      | IF Expr THEN Expr ELSE Expr  %prec prec_if
          { Cond($2, $4, $6) }
      | IF Expr THEN Expr  %prec prec_if
          { Cond($2, $4, Const Unit)}
      | LET IDENT EQUAL Expr IN Expr  %prec prec_let
          { Letin (false,$2,$4,$6) }
      | LET REC IDENT EQUAL Expr  IN Expr  %prec prec_let
          { Letin (true,$3,$5,$7) }
      | FUNCTION IDENT MINUSGREATER Expr
          { Abs($2,$4) }
      | FUN IDENT MINUSGREATER Expr
          { Abs($2,$4) }
;
Simple_expr :
        Struct_constant
          { Const $1 }
      | IDENT
	  {Var $1}
      | LPAREN RPAREN
          { Const  Unit }
      | LBRACKET RBRACKET
         { Const Emptylist}
      | LPAREN Expr RPAREN
          { $2 }
      | PREFIX Simple_expr
          { Unop ( $1,$2)}
;


Simple_expr_list :
        Simple_expr Simple_expr_list
          { $1 :: $2 }
      | Simple_expr
          { [$1] }
;
Struct_constant :
        INT
          { Int $1 }
      | FLOAT
          { Float $1 }
      | STRING
          { String  $1 }
;


%%



