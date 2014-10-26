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


let getId id = getElemFromID id

let getButton id = getId id Dom_html.CoerceTo.button

let getTextArea id = getId id Dom_html.CoerceTo.textarea

let getTbody id = getId id Dom_html.CoerceTo.tbody

let getTable id = getId id Dom_html.CoerceTo.table

let empty item = item##innerHTML <- ""

let writeInTextArea ta msg = 
  ta##value <- ta##value##concat (Js.string msg)

let newTbod () = 
  let tb = (createTbody window##document) in
  tb##id<-Js.string "currentType";
  tb

let print_newline ta = 
  writeInTextArea ta "\n"

let readFromTextArea ta = 
  Js.to_string ta##value

let readLastExpr ta =
  let str = ta##value |> Js.to_string in
  String.rindex str '$' 
  |> fun i -> String.sub str i (String.length str - i -1)

let makeTd str = 
  let td = createTd window##document in
  td##innerHTML <- Js.string str;
  td

(* Type to_string *)

let string_of_consttype = function 
  Int_type -> "int"
| Float_type -> "float"
| String_type -> "string"
| Bool_type -> "bool"
| Unit_type -> "unit"

let string_of_quantified_type (Forall(gv,t)) = 
  let names = 
    let rec names_of = function
      (n,[]) -> []
    | (n,(v1::lv)) -> (get_name t n)::(names_of (n+1,lv))
    in (names_of (1,gv))
  in 
    let var_names = combine (rev gv) names
    in 
      let rec string_rec exp =
	let res = ref "" in
       match exp with
         Var_type {contents=(Instanciated t)} -> string_rec t 
      |  Var_type {contents=(Unknown n)} -> 
           let name = (try assoc n var_names 
                       with Not_found -> raise (Failure "Non quantified variable in type jstool78"))
           in res:=!res^name;!res
      |  Var_type {contents=(Weak n)} -> 
           let name = (try assoc n var_names 
                       with Not_found -> raise (Failure "Non weak variable in type jstool82"))
           in res:=!res^name;!res
      | Const_type ct -> res:= (!res^(string_of_consttype ct));!res
      | Pair_type(t1,t2) -> res:= (!res^"("^(string_rec t1)
                            ^" * "^(string_rec t2)^")");!res
      | List_type t ->  res:= !res^"(("^(string_rec t)^") list)";!res

      | Fun_type(t1,t2)  ->res:= !res^"("^(string_rec t1)
                            ^" -> "^(string_rec t2)^")";!res
      | Ref_type t -> res:= !res^ "(("^( string_rec t)^") ref)";!res
      in 
        string_rec t
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
    | (n,(v1::lv)) -> (get_name t n)::(names_of (n+1,lv))
    in (names_of (1,gv))
  in 
    let var_names = combine (rev gv) names
    in 
      let rec print_rec = function 
         Var_type {contents=(Instanciated t)} -> print_rec t 
      |  Var_type {contents=(Unknown n)} -> 
           let name = (try assoc n var_names 
                       with Not_found -> raise (Failure "Non quantified variable in type jstool116"))
           in writeInTextArea ta name
      |  Var_type {contents=(Weak n)} -> 
           let name = (try assoc n var_names
                       with Not_found -> raise (Failure "Non weak variable in type jstool120"))
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

let empty tp = 
  for i = 0 to !nb_added do
    tp##deleteRow(0)
  done

let print_tr couple = 
  let fsttd = makeTd (fst couple)
  and sndtd = makeTd (string_of_quantified_type (snd couple))
  and doc = window##document
  in let tr = createTr doc in
     Dom.appendChild tr fsttd;
     Dom.appendChild tr sndtd;
     tr

let setCaretPos elem num = 
  Js.Unsafe.fun_call (Js.Unsafe.variable "setCaretPosition") 
    [|Js.Unsafe.inject elem;Js.Unsafe.inject num|]

let print_current_env tp =
  Dom.insertBefore tp (print_tr (List.hd !initial_typing_env)) tp##firstChild
