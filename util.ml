(************************************************************************
 *                                                                      *
 *                   MASTER STL - M2 -  anne'e 2014/2015                *
 *                                                                      *
 *                                Cours TAS                             *
 *                                                                      *
 *                                                                      *
 ************************************************************************
 *                                                                      *
 *   auteur : Emmanuel Chailloux                                        *
 *                                                                      *
 ************************************************************************)

(* passage O'Caml *)

(* pour forcer l'ordre d'e'valuation du map on introduit un let in *)

let rec map f l = match l with 
  [] -> []
| t::q -> let u = f t in u::(map f q)
;;


(* une fonction de nettoyage de fichiers *)
let remove_file f =
  try
    Sys.remove f
  with Sys_error _ ->
    ()
;;


let rec subtract s1 s2 = match s1 with 
  []-> []
| t::q -> if List.mem t s2 then subtract q s2 else t::(subtract q s2)
;;

let list_it = List.fold_right;;
let it_list = List.fold_left;;
let do_list = List.iter;;
let assoc = List.assoc;;
let mem = List.mem;;
let combine = List.combine;;
let rev = List.rev;;
let create_string = String.create;;
let string_length = String.length;;
let blit_string = String.blit;;
let set_nth_char = String.set;;
let sub_string = String.sub;;
let get_lexeme_char = Lexing.lexeme_char;;
let get_lexeme = Lexing.lexeme;;


exception Error of exn * int * int;;
exception Unterminated_string;;
exception Unterminated_comment;;
exception Unterminated_stringt;;
exception Bad_char_constant;;
exception Illegal_character;;
