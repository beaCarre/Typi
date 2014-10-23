open Jstool
open Util
open Types
open Alex
open Asyn
open Typeur
open Env_typeur

let getButton id = getElemFromID id Dom_html.CoerceTo.button

let getTextArea () = getElemFromID "console" Dom_html.CoerceTo.textarea

let writeInTextArea ta msg = 
  ta##value <- ta##value##concat (Js.string msg)

let print_newline ta = 
  writeInTextArea ta "\n"

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

let init console = 
  let clearButton = getButton "buttonClear"
  in
  clearButton##onclick <- Dom_html.handler 
    (fun evt -> 
      console##value <- (Js.string "");
      Js._true)

let main ta = 
  try
    let lexbuf = Lexing.from_channel stdin in 
    while true do 
      try 
	writeInTextArea ta "$ ";
	let result = Asyn.implementation Alex.main lexbuf in
	begin
          match result with 
            Expr e ->  
              let ne,qt = type_check e in 
              begin 
		writeInTextArea ta "- : ";
		print_quantified_type ta qt; 
		print_newline ta
              end
          | Decl (Let(b,s,e)) -> 
            let e = 
              if not b then e
              else (Letin(b,s,e,Var s))
            in 
            let ne,qt = type_check e in
            begin 
             writeInTextArea ta (s^" : "); print_quantified_type ta qt; 
              print_newline ta; flush stdout;
              add_initial_typing_env (s,qt)
            end
	end
      with Failure "type_check" ->  writeInTextArea ta "Erreur de typage"; print_newline ta
      | Toplevel -> ()
      | Failure s -> writeInTextArea ta ("Erreur " ^ s); print_newline ta
      | Parsing.Parse_error -> writeInTextArea ta "Erreur de syntaxe"; print_newline ta
      | _ -> () 
    done
  with End_of_file -> ()
    
let go _ =
  let console = getTextArea () in
  init console;
  main console;
  Js._true

let _ = 
  Dom_html.window##onload <- Dom_html.handler go
