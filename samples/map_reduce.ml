let rec map = fun f -> fun l -> 
  if l = [] then
    []
  else 
    (f (hd l)) :: (map f (tl l))

let rec fold_left = fun f_red -> fun acc -> fun l ->
  if l = [] then
    acc
  else 
    fold_left f_red (f_red acc (hd l)) (tl l)
