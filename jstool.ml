open Dom_html
open Lwt
open Js

open Typeur
open Types
open Util

open Env_typeur

let getElemFromID id coerceCible =
    Opt.get
      (Opt.bind (Dom_html.document##getElementById(string id))
         coerceCible)
      (fun () -> failwith ("this id : "^id^" does not match any element"))


let getId = fun id -> getElemFromID id

let getButton id = getId id Dom_html.CoerceTo.button

let getTextArea id = getId id Dom_html.CoerceTo.textarea

let writeInTextArea ta msg = 
  ta##value <- ta##value##concat (Js.string msg)

let print_newline ta = 
  writeInTextArea ta "\n"

let readFromTextArea ta = 
  Js.to_string ta##value

let readAndEscapeText ta =
  Js.to_string ta##value


(*********** Type printing ***************)

let print_consttype ta = function 
  Int_type -> writeInTextArea ta "int"
| Float_type -> writeInTextArea ta "float"
| String_type -> writeInTextArea ta "string"
| Bool_type -> writeInTextArea ta "bool"
| Unit_type -> writeInTextArea ta "unit"

let print_quantified_type ta (Forall(gv,t)) = 
  let names = 
    let rec names_of = function
      (n,[]) -> []
    | (n,(v1::lv)) -> (var_name n)::(names_of (n+1,lv))
    in (names_of (1,gv))
  in 
    let var_names = combine (rev gv) names
    in 
      let rec print_rec = function 
         Var_type {contents=(Instanciated t)} -> print_rec t 
      |  Var_type {contents=(Unknown n)} -> 
           let name = (try assoc n var_names 
                       with Not_found -> raise (Failure "Non quantified variable in type"))
           in writeInTextArea ta name
      | Const_type ct -> print_consttype ta ct
      | Pair_type(t1,t2) -> writeInTextArea ta "("; print_rec t1;
                            writeInTextArea ta " * "; print_rec t2;  writeInTextArea ta ")"
      | List_type t -> writeInTextArea ta "(("; print_rec t; writeInTextArea ta ") list)"
      | Fun_type(t1,t2)  -> writeInTextArea ta "("; print_rec t1;
                            writeInTextArea ta " -> "; print_rec t2; writeInTextArea ta ")"
      | Ref_type t -> writeInTextArea ta "(("; print_rec t; writeInTextArea ta ") ref)"
      in 
        print_rec t



let print_current_env ta =
  List.iter 
    (fun e -> writeInTextArea ta ((fst e)^" : ");
              print_quantified_type ta (snd e);
              writeInTextArea ta "\n\n")
    !initial_typing_env
