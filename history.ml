exception NoPrevious
exception NoNext
exception EmptyHistory

let isZero = ref false

type 'a history = {all:'a list; index:int}

type 'a t = Empty | H of 'a history

let init () = 
  Empty

let add h x = 
  match h with 
  | Empty -> H({all=[x]; index=(-1)})
  | H(h) -> H({all=x::h.all;index=(-1)}) 

let length h = 
  match h with
  | Empty -> -1
  | H(h) -> List.length h.all

let getCurrent h =
  match h with
  | Empty -> raise EmptyHistory
  | H(h) -> List.nth h.all h.index

let prev h =
  match h with
  | Empty -> raise NoPrevious
  | H(h) -> 
    if !isZero then begin
      isZero := false;
      H ({all=h.all; index = 0})
    end else H ({all=h.all; index= min ((List.length h.all) -1) (h.index + 1)})

let next h =
  match h with
  | Empty -> raise NoNext
  | H(h) -> 
    if h.index <= 0 then begin isZero := true; raise NoNext end
    else H ({all=h.all;index = (h.index - 1)})
