let id = fun x -> x;;

let succ = fun x -> x+1;;

let ref_f = ref id;;

ref_f:=succ;;

!ref_f;; 
