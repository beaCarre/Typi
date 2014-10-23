open Util;;
open Types;;
open Alex;;
open Asyn;;
open Typeur;;



let initial_typing_env = 
ref(  let mk_type (ct1,ct2,ct3) = 
    Forall([], Fun_type (Pair_type(Const_type ct1, Const_type ct2),Const_type ct3))
  in 
    let int_ftype = mk_type(Int_type,Int_type,Int_type)
    and float_ftype = mk_type(Float_type,Float_type,Float_type)
    and int_predtype = mk_type(Int_type,Int_type,Bool_type)
    and float_predtype = mk_type(Float_type,Float_type,Bool_type)
    and alpha = Var_type(ref(Unknown 1))
    and beta = Var_type(ref(Unknown 2))
    in 
      ("=",Forall([1],Fun_type (Pair_type (alpha,alpha), 
                                Const_type Bool_type)))::   
      ("true", Forall([],Const_type Bool_type)) ::
      ("false", Forall([],Const_type Bool_type)) ::     
      (map (function s -> (s,int_ftype)) ["*";"+";"-";"/"]) @
      (map (function s -> (s,float_ftype)) ["*.";"+.";"-.";"/."]) @
      (map (function s -> (s,int_predtype)) ["<";">";"<=";">="]) @
      (map (function s -> (s,float_predtype)) ["<.";">.";"<=.";">=."]) @
      ["^", mk_type (String_type, String_type, String_type)] @
      [("hd",Forall([1], Fun_type (List_type alpha, alpha)));
       ("tl",Forall([1], Fun_type (List_type alpha, List_type alpha)));
       ("fst",Forall([1;2], Fun_type (Pair_type (alpha,beta),alpha)));
       ("snd",Forall([1;2], Fun_type (Pair_type (alpha,beta),beta)));
       ("ref",Forall([1], Fun_type (alpha, Ref_type alpha)));
       ("!",  Forall([1], Fun_type (Ref_type alpha, alpha)));
       (":=",  Forall([1], Fun_type (Pair_type(Ref_type alpha, alpha),Const_type Unit_type)))
])
;;

let add_initial_typing_env (name,typ) = 
    initial_typing_env := (name,typ) :: (!initial_typing_env)
;;

let type_check e = 
  let et = typing_handler type_expr !initial_typing_env e 
  in 
    let t =  et in 
    let qt = snd(List.hd(generalize_types !initial_typing_env ["_zztop",t]))
    in 
      et,qt
;;



exception Toplevel;;


let eprintf a  = print_string a;;



let parse_phrase parsing_fun lexing_fun lexbuf =
  let rec skip () =
    try
      match lexing_fun lexbuf with
        EOF -> ()
      | SEMISEMI -> ()
      | _ -> skip()
    with Error(_,_,_) ->
      skip() in
  let skip_maybe () =
    if Parsing.is_current_lookahead EOF
    or Parsing.is_current_lookahead SEMISEMI
    then () else skip() in


  try
    parsing_fun lexing_fun lexbuf
  with Parsing.Parse_error ->
         let pos1 = Lexing.lexeme_start lexbuf in
         let pos2 = Lexing.lexeme_end lexbuf in
         skip_maybe();
         eprintf "Syntax error.\n";
         raise Toplevel
    | Error(errcode, pos1, pos2) ->
         let l = (pos1, pos2) in
         begin match errcode with
           Illegal_character ->
             eprintf "Illegal character.\n" 
         | Unterminated_comment ->
             eprintf "Comment not terminated.\n" 
         | Bad_char_constant ->
             eprintf "Ill-formed character literal.\n"
         | Unterminated_string ->
             eprintf "String literal not terminated.\n"
         end;
         skip();
         raise Toplevel
;;

let parse_impl_phrase = parse_phrase Asyn.implementation Alex.main;;
