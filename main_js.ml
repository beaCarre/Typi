open Jstool

let getButton id = getElemFromID id Dom_html.CoerceTo.button

let getTextArea () = getElemFromID "console" Dom_html.CoerceTo.textarea

let init () = 
  let console = getTextArea () 
  and clearButton = getButton "buttonClear"
  in
  clearButton##onclick <- Dom_html.handler 
    (fun evt -> 
      console##value <- (Js.string "");
      Js._true;)

let go _ =
  init ();
  Js._true

let _ = 
  Dom_html.window##onload <- Dom_html.handler go
