open Dom_html
open Lwt
open Js

let getElemFromID id coerceCible =
    Opt.get
      (Opt.bind (Dom_html.document##getElementById(string id))
         coerceCible)
      (fun () -> failwith ("this id : "^id^" does not match any element"))
