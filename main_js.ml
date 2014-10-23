open Util
open Types
open Alex
open Asyn
open Typeur
open Env_typeur
open Jstool
open Keyboard

let go_type_baby ta tp= 
  try
    let lexbuf = Lexing.from_string (readFromTextArea ta) in
    writeInTextArea ta "\n$ ";
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
          add_initial_typing_env (s,qt);
	  print_current_env tp
	end
    end

  with Failure "type_check" -> writeInTextArea ta "Erreur de typage"; print_newline ta
  | Toplevel -> ()
  | Failure s -> writeInTextArea ta ("Erreur " ^ s); print_newline ta
  | Parsing.Parse_error -> writeInTextArea ta "Erreur de syntaxe"; print_newline ta
  | _ -> () 

let init console typeCur = 
  let clearButton = getButton "buttonClear"
  and resetButton = getButton "buttonReset"
  in
  resetButton##onclick <- Dom_html.handler 
    (fun _ ->
      initial_typing_env:=init_env();
      print_current_env typeCur;
      Js._true);
  clearButton##onclick <- Dom_html.handler 
    (fun evt -> 
      console##value <- (Js.string "");
      Js._true);
  console##onkeydown <- Dom_html.handler 
      (fun evt -> 
	let key = evt##keyCode in
	if key=13 then go_type_baby console typeCur;
	Js._true;)
 
let go _ =
  let console = getTextArea "console" 
  and typeCur = getTable "currentType" in
  init console typeCur;
  Js._true

let _ = 
  Dom_html.window##onload <- Dom_html.handler go
