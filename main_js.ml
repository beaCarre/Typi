open Util
open Types
open Alex
open Asyn
open Typeur
open Env_typeur
open Jstool
open Keyboard

type zippo = {mutable cmds: string list
	     ; mutable idx: int
	     ; mutable size: int}

let historique = {cmds=[]; idx=0; size=0}

let go_type_baby in_console out_console tp = 
  begin try
	  let entry = readFromTextArea in_console in
	  historique.cmds <- entry::historique.cmds;
	  historique.size <- historique.size + 1;
	  historique.idx <- 0;
	  let lexbuf = Lexing.from_string entry in
	  let l_result = Asyn.start Alex.main lexbuf in
	  List.iter (function
	  | Expr e ->  
	    let ne,qt = type_check e true in 
	    begin 
	      writeInTextArea out_console "\n - ";
	      writeInTextArea out_console ": ";
	      print_quantified_type out_console qt; 
	    end
	  | Decl (Let(b,s,e)) -> 
	    let e = 
	      if not b then e
	      else (Letin(b,s,e,Var s))
	    in
	    let ne,qt = type_check e true in
	    writeInTextArea out_console "\n - ";
	    writeInTextArea out_console (s^" : "); 
	    add_initial_typing_env (s,qt);
	    print_quantified_type out_console qt;
	    print_current_env tp
	  ) l_result
    with Failure "type_check" -> 
      writeInTextArea out_console "\n - ";  
      writeInTextArea out_console "Erreur de typage"
    | Toplevel -> ()
    | Failure s -> 
      writeInTextArea out_console "\n - ";
      writeInTextArea out_console ("Erreur " ^ s)
    | Parsing.Parse_error -> 
      writeInTextArea out_console "\n - "; 
      writeInTextArea out_console "Erreur de syntaxe"
  end;
  out_console##scrollTop <- out_console##scrollHeight;
  in_console##value <- (Js.string "")


let init in_console out_console typeCur =
  let multiline = ref false in
  let clearButton = getButton "buttonClear"
  and resetButton = getButton "buttonReset"
  and sendButton = getButton "buttonSend"
  in
  sendButton##onclick <- Dom_html.handler 
    (fun _ -> 
      go_type_baby in_console out_console typeCur;
      Js._true);
  resetButton##onclick <- Dom_html.handler 
    (fun _ ->
      initial_typing_env:=init_env();
      empty typeCur;
      nb_added:=0;
      Js._true);
  clearButton##onclick <- Dom_html.handler 
    (fun evt -> 
      out_console##value <- (Js.string "");
      Js._true);
  in_console##onkeydown <- Dom_html.handler 
    (fun evt -> 
      let key = evt##keyCode in
      if key=13 && (not !multiline) then begin go_type_baby in_console out_console typeCur;
	Dom_html.stopPropagation evt
      end;
      if key=16 then multiline := true;
      Js._true);
  in_console##onkeyup <- Dom_html.handler 
    (fun evt -> 
      let key = evt##keyCode in
      if key=16 then multiline := false;
      let put_historic_value () =
	match historique.idx with
	| x when x < 0 ->
	  historique.idx <- -1;
	  in_console##value <- Js.string ""
	| x when x > historique.size - 1 ->
	  historique.idx <- historique.size;
	  in_console##value <- Js.string ""
	| x -> let str = 
		 List.nth historique.cmds (historique.size - 1 - x)
	       in
	       in_console##value <- Js.string str
      in
      if key=40 && !multiline then 
	begin
	  historique.idx <- historique.idx - 1;
	  put_historic_value ()
	end
      ;
      if key=38 && !multiline then 
	begin
	  historique.idx <- historique.idx + 1;
	  put_historic_value ()
	end
      ;

      if key = 13 && not !multiline then
	begin
	  in_console##value <- (Js.string "")
        end
      ;
     
      Js._true;)
 
let go _ =
  let out_console = getTextArea "out" 
  and in_console = getTextArea "in" 
  and typeCur = getTbody "currentType" in
  init in_console out_console typeCur;
  Js._true

let _ = 
  Dom_html.window##onload <- Dom_html.handler go
