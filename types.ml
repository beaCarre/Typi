type vartype = 
  Unknown of int
| Weak of int
| Instanciated of ml_type
and consttype = 
  Int_type
| Float_type
| String_type
| Bool_type
| Unit_type
and ml_type = 
  Var_type of vartype ref
| Const_type of consttype
| Pair_type of ml_type * ml_type
| List_type of ml_type
| Fun_type of ml_type * ml_type
| Ref_type of ml_type
;;

type ml_expr = Const of ml_const
|  Var of string
|  Unop of string  * ml_expr
|  Binop of string  * ml_expr * ml_expr
|  Pair of ml_expr * ml_expr 
|  Cons of ml_expr * ml_expr
|  Cond of ml_expr * ml_expr * ml_expr
|  App of ml_expr * ml_expr
|  Abs of string * ml_expr
|  Letin of bool * string * ml_expr * ml_expr
|  Ref of ml_expr

and ml_const = Int of int
| Float of float
| Bool of bool
| String of string
| Emptylist
| Unit
;;

type ml_decl = 
  Let of bool * string * ml_expr
;;


type ml_phrase = 
  Expr of ml_expr
| Decl of ml_decl 
;;






