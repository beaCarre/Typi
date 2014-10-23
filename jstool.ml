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

let getTable id = getId id Dom_html.CoerceTo.tbody

let writeInTextArea ta msg = 
  ta##value <- ta##value##concat (Js.string msg)

let print_newline ta = 
  writeInTextArea ta "\n"

let readFromTextArea ta = 
  Js.to_string ta##value

let readAndEscapeText ta =
  Js.to_string ta##value

let makeTd str = 
  let td = createTd window##document in
  td##innerHTML <- Js.string str;
  td

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


(*************** Printing the current environement  *********************)


let get_added() = 
  let rec added_aux res fstsize = 
    if List.length !initial_typing_env = fstsize then [] else
      match !initial_typing_env with
      | [] -> res
      | h::tl -> added_aux (List.append res [h]) (fstsize - 1)
  in
  added_aux [] initial_size

let print_tr couple = 
  let fsttd = makeTd (fst couple)
  and sndtd = makeTd ("")
  and doc = window##document
  in let tr = createTr doc in
     Dom.appendChild tr fsttd;
     Dom.appendChild tr sndtd;
     tr

let print_current_env tp =
  List.iter (fun e -> Dom.appendChild tp (print_tr e)) !initial_typing_env
