open Util
open Types


let new_unknown,new_weak,reset_unknowns,max_unknown = 
  let c1 = ref 1 
  and c2 = ref 1
  and max = ref 10000 
  in 
   ( (function () -> c1:=!c1+1; if (!c1)+(!c2) >= !max then failwith "No more types";
                     Var_type( ref(Unknown !c1))),
     (function () -> c2:=!c2+1; if (!c2)+(!c1) >= !max then failwith "No more types";
                     Var_type( ref(Weak !c2))),
     (function () -> c1:=1;c2:=1),
     (function () -> Var_type(ref(Unknown !max))))


type quantified_type = Forall of (int list) * ml_type

let rec vars_of_type t =
   let rec vars vl = function
     Const_type _ -> vl
   | Var_type vt ->
       ( match !vt with
           Unknown n -> if List.mem n vl then vl else n::vl
         | Weak n -> vl
         | Instanciated t -> vars vl t
       )
   | Pair_type (t1,t2) -> vars (vars vl t1) t2
   | List_type t -> vars vl t
   | Fun_type (t1,t2) -> vars (vars vl t1) t2
   | Ref_type t -> vars vl t
   in
     vars [] t 

let subtract l1 l2 =
   List.flatten (List.map (function id ->
                             if (List.mem id l2) then [] else [id])
                  l1)

let free_vars_of_type (bv,t) =
   subtract (vars_of_type t) bv
 and bound_vars_of_type (fv,t) =
   subtract (vars_of_type t) fv 

let flat ll = List.fold_right (@) ll []

let free_vars_of_type_env l =
     flat ( List.map (function (id,Forall (v,t))
                        -> free_vars_of_type (v,t)) l) 

let type_instance st =
  match st with Forall(gv,t) -> 
    let unknowns = List.map (function n -> n,new_unknown()) gv
    and weaks = List.map (function n -> n,new_weak()) gv
    in
    let rec instance = function
      | Var_type {contents=(Unknown n)} as t ->
        (try List.assoc n unknowns with Not_found -> t)
      | Var_type {contents=(Weak n)} as t ->  
	(try List.assoc n weaks with Not_found -> t)
      | Var_type {contents=(Instanciated t)} -> instance t
      | Const_type ct as t -> t
      | Pair_type (t1,t2) -> Pair_type (instance t1, instance t2)
      | List_type t -> List_type (instance t)
      | Fun_type (t1,t2) -> Fun_type (instance t1, instance t2)
      | Ref_type t -> Ref_type (instance t)
    in
    instance t

type typing_error =
   Unbound_var of string
 | Clash of ml_type * ml_type 

exception Type_error of typing_error

let occurs n t = List.mem n (vars_of_type t)

let rec shorten = function
     Var_type (vt) as tt ->
       (match !vt with
            Unknown _  | Weak _ -> tt
          | Instanciated ((Var_type _) as t) ->
              let t2 = shorten t in
                vt := Instanciated t;
                t2
          | Instanciated t -> t
       )
   | t -> t

let rec unify_types (t1,t2) =
    let lt1 = shorten t1 and lt2 = shorten t2
    in
      match (lt1,lt2) with
      | Var_type ({contents=Weak n} as occn),
        Var_type {contents=Instanciated _} ->
          occn:= Instanciated lt2
      | Var_type ({contents=Weak n} as occn),
        Var_type {contents=Weak m} ->
          if n=m then () else occn:= Instanciated lt2
      | Var_type ({contents=Unknown n} as occn),
        Var_type {contents=Unknown m} ->
          if n=m then () else occn:= Instanciated lt2
      | Var_type ({contents=(Unknown n)} as occn), _ -> 
          if occurs n lt2
          then raise (Type_error(Clash(lt1,lt2)))
          else occn:=Instanciated lt2
      | _ , Var_type ({contents=(Unknown n)}) -> unify_types (lt2,lt1)    | _ , Var_type ({contents=(Weak n)}) -> unify_types (lt2,lt1)    
      | Const_type ct1, Const_type ct2 ->
          if ct1=ct2 then () else raise (Type_error(Clash(lt1,lt2)))
      | Pair_type (t1,t2), Pair_type (t3,t4) ->
          unify_types (t1,t3); unify_types(t2,t4)
      | List_type t1, List_type t2 ->  unify_types (t1,t2)
      | Fun_type (t1,t2), Fun_type (t3,t4) -> 
         unify_types (t1,t3); unify_types(t2,t4)
      | Ref_type t1, Ref_type t2 ->  unify_types (t1,t2)
      | _ ->  raise(Type_error(Clash(lt1,lt2)))

let type_const = function
   Int _ -> Const_type Int_type
 | Float _ -> Const_type Float_type
 | String _ -> Const_type String_type
 | Bool _ ->  Const_type Bool_type
 | Unit ->  Const_type Unit_type
 | Emptylist -> List_type (new_unknown())

let isExpansive expr =
  match expr with
  | App (_,_) | Ref _ -> failwith "expansive"; true
  | _  -> failwith "non-expansive" ;false

let generalize_types gamma l =
   let fvg = free_vars_of_type_env gamma
   in
     List.map (function (s,t) ->
                (s, Forall(free_vars_of_type (fvg,t),t))) l
 
let rec type_expr gamma =
   let rec type_rec expri generalisation = 
     match expri with
       Const c -> type_const c
     | Var s -> let t = try List.assoc s gamma
       with Not_found -> raise (Type_error(Unbound_var s))
		in  type_instance t
     | Unop (s,e) ->
       let t = try assoc s gamma
         with Not_found -> raise (Type_error(Unbound_var s))
       in 
       let t1 = type_instance t
       and t2 = type_rec e generalisation in
       let u = new_unknown()
       in
       unify_types(t1, Fun_type(t2,u)); u
	 
     | Binop(s,e1,e2) ->
       let t = try assoc s gamma
         with Not_found -> raise (Type_error(Unbound_var s))
       in
       let t0 = type_instance t
       and t1 = type_rec e1 generalisation
       and t2 = type_rec e2 generalisation
       in
       let u = new_unknown()
       and v = new_unknown()
       in
       unify_types(t0, Fun_type(Pair_type (t1,t2),u));
       u
	 
     | Pair (e1,e2) -> Pair_type (type_rec e1 generalisation, type_rec e2 generalisation)
     | Cons (e1,e2) ->
       let t1 = type_rec e1 generalisation
       and t2 = type_rec e2 generalisation in
       unify_types (List_type t1, t2); t2
     | Cond (e1,e2,e3) ->
       let t1 = unify_types (Const_type Bool_type, type_rec e1 generalisation)
       and t2 = type_rec e2 generalisation
       and t3 = type_rec e3 generalisation in 
       unify_types (t2,t3); t2
     | App (e1,e2) ->
       let t1 = type_rec e1 generalisation
       and t2 = type_rec e2 generalisation in
       let u = new_unknown() in
       unify_types (t1, Fun_type (t2,u)); u
     | Abs(s,e) ->
       let t = new_unknown() in
             let new_env = (s,Forall ([],t))::gamma in
             Fun_type (t, type_expr new_env e generalisation)
     | Letin (false,s,e1,e2) ->
	 (* let s = e1 in e2 *)
	 (* false pour non récursive *)
       let t1 = type_rec e1 generalisation in
       if  not (isExpansive e1) then 
         let new_env = generalize_types gamma [ (s,t1) ] in
         type_expr (new_env@gamma) e2 generalisation
       else
	 begin
           type_expr ((s,(Forall([],t1))) :: gamma) e2 generalisation 
	 end
     | Letin (true,s,e1,e2) -> 
	   (* true pour recursive *)
           let u = new_unknown () in
           let new_env = (s,Forall([  ],u))::gamma in
           let t1 = type_expr (new_env@gamma) e1 generalisation in
           if  not (isExpansive e1) then 
	     let new_env = generalize_types gamma [ (s,t1) ] in
	     type_expr (new_env@gamma) e2 generalisation
           else
	     begin
               type_expr ((s,(Forall([],t1))) :: gamma) e2 generalisation
	     end
     | Ref e -> Ref_type (type_rec e generalisation)
   in
   type_rec
     
(*************************** Printing **************************************)

(*let print_consttype = function 
  Int_type -> print_string   "int"
| Float_type -> print_string "float"
| String_type -> print_string "string"
| Bool_type -> print_string "bool"
| Unit_type -> print_string "unit"
*)
let ascii i = let s = String.create 1 in s.[0] <- Char.chr  i;s

let var_name n = 
  let rec name_of n = 
     let q,r = ((n / 26), (n mod 26))
     in 
        if q=0 then ascii (96+r)
        else (name_of q)^(ascii (96+r))
   in "'"^(name_of n)

let weakvar_name n = 
  let rec name_of n = 
     let q,r = ((n / 26), (n mod 26))
     in 
        if q=0 then ascii (96+r)
        else (name_of q)^(ascii (96+r))
   in "'_"^(name_of n)

let get_name t n =
  match t with
  |  Var_type {contents=(Weak _)} -> weakvar_name n 
  | _ -> var_name n

(*
let print_quantified_type (Forall(gv,t)) = 
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
                       with Not_found -> raise (Failure "Non quantified variable in type"))
           in print_string name
      |  Var_type {contents=(Weak n)} -> 
           let name = (try assoc n var_names 
                       with Not_found -> raise (Failure "Non quantified variable in type"))
           in print_string name
      | Const_type ct -> print_consttype ct
      | Pair_type(t1,t2) -> print_string "("; print_rec t1;
                            print_string " * "; print_rec t2; print_string ")"
      | List_type t -> print_string "(("; print_rec t; print_string ") list)"
      | Fun_type(t1,t2)  -> print_string "("; print_rec t1;
                            print_string " -> "; print_rec t2; print_string ")"
      | Ref_type t -> print_string "(("; print_rec t; print_string ") ref)"
      in 
        print_rec t


let print_type t = print_quantified_type (Forall(free_vars_of_type ([],t),t))

*)

let typing_handler typing_fun env expr = 
  reset_unknowns();
  try typing_fun env expr 
  with 
    Type_error (Clash(lt1,lt2)) -> 
        failwith "type_check"
  | Type_error (Unbound_var s)  ->
        failwith "type_check"

(*

let type_check e = 
  let t = typing_handler type_expr initial_typing_env e 
  in 
    let qt = snd(hd(generalize_types initial_typing_env ["it",t]))
    in 
      print_string "it : "; print_quantified_type qt; print_newline()

*)

(*

type_check (Const (Int 3))
type_check (Const (Float 3.2))
type_check (Abs ("x", Pair(Var "x", Var "x")))
type_check (Cond (App (Var "=", Pair(Const(Int 0), Const (Int 0)))
                 , Const(Int 2)
                 , Const(Int 5)))

type_check (Letin (true,"fact",
Abs ("x", Cond (App (Var "=",Pair(Var "x",Const(Int 1))) 
                ,Const(Int 1) 
                ,App (Var "*"
                      ,Pair(Var "x"
                           ,App (Var "fact"
                                 ,App (Var"-"
                                       ,Pair(Var "x"
                                             ,Const(Int 1) ))))) ))
, App (Var "fact", Const (Int 4)))) 

*)
