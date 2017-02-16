(* vim: set ft=ocaml tabstop=2 shiftwidth=2 commentstring=(*\ %s\ *): *)
(* 
   CS131 Homework 1 - Gautam Gupta #304282688 
   Worked with: Kelly Hosokawa (on powerset function)
   Other resources used: None
*)
exception ImplementMe

(* Problem 1 *)

let rec (member: 'a -> 'a list -> bool) = (fun x s ->
    match s with
      [] -> false
    | h::t -> if x=h then true else member x t
  )

let (add : 'a -> 'a list -> 'a list) = (fun x s ->
    if (member x s) then s else x::s)

let rec (union : 'a list -> 'a list -> 'a list) = (fun s1 s2 ->
    match s1 with
      [] -> s2
    | h::t -> union t (add h s2)
  )

let rec (fastUnion : 'a list -> 'a list -> 'a list) = (fun s1 s2 ->
    match s1 with 
      [] -> s2
    | h1::t1 ->
        (match s2 with
           [] -> s1
         | h2::t2 ->
             (if h1=h2 then (fastUnion t1 s2) else
              if h1<h2 then (fastUnion t1 (h1::s2)) else
                h2::(fastUnion s1 t2)
             )
        )
  )

let (intersection : 'a list -> 'a list -> 'a list) = (fun s1 s2 ->
    List.filter (fun x -> member x s2) s1
  )

let rec (setify : 'a list -> 'a list) = (fun l ->
    match l with 
      [] -> []
    | h::t -> if member h t then setify t else h::(setify t)
  )

let rec (powerset : 'a list -> 'a list list) = (fun l ->
    match l with
      [] -> [[]]
    | h::t -> let ps = (powerset t) in (ps @ (List.map (fun x -> add h x) ps))
  )


(* Problem 2 *)

let rec (partition : ('a -> bool) -> 'a list -> 'a list * 'a list) = (fun f l ->
    match l with
      [] -> ([],[])
    | h::t -> let (yes,no) = partition f t in (
        if f h then (h::yes, no) else (yes, h::no)
      )
  )

let rec (whle : ('a -> bool) -> ('a -> 'a) -> 'a -> 'a) = (fun p f x ->
    match (p x) with 
      false -> x
    | true -> whle p f (f x)
  )

let rec (pow : int -> ('a -> 'a) -> ('a -> 'a)) = (fun n f ->
    match n with
      0 -> (fun v -> v)
    | i -> fun v -> f (pow (i-1) f v) 
  )

