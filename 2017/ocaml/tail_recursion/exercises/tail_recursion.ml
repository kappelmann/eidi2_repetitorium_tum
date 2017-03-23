(* 1. Sie erstellen eine laufzeitkritische Anwendung. Welche der folgenden Definitionen würden sie bevorzugen. Warum? *)
let rec append_1 x y = match x with [] -> y | x::xs -> x::append_1 xs y

let append_2 x y = let rec aux acc = function [] -> acc
        | x::xs -> aux (x::acc) xs
        in aux y (List.rev x)

let append_3 x y = x@y

(* 2. Implementieren Sie folgende Funktionen endrekursiv *)
(* Sie dürfen für gauss x>=0 und für h p>=0 annehmen. *)
let rec gauss = function x when x>1 -> gauss (x-1)+x
	| x -> x

let gauss n = (n*(n+1))/2

let gauss n = let rec aux acc = function n when n>0 -> aux (acc+n) (n-1)
        | n -> acc
        in aux 0 n

let rec h e l p = match p with 0 -> (p+1)
	| x -> e*l*h e l (x-1)

let g f l = let rl = List.rev l in 
	List.fold_right f rl 0

let rec a x y = match (x,y) with ([],_) -> y | (_,[]) -> x | (x::xs,_) -> x::a xs y

let fold_right f l acc = List.fold_right f l acc

let fold_right = List.fold_left f acc (List.rev l)

(* 3. Sind folgende Funktionen endrekursiv? Begründung! *)

let f1 a b = a*b

let rec f2 a b = f2 a b ; (f2 a b)


let f3 a b = f2 a b

let rec f4 = function [] -> 1
	| x::xs -> -f4 xs

let rec f5 = function [] -> 1
	| x::xs -> f6 xs
and f6 = function [] -> 2
	| x::xs -> f5 xs 

let rec f7 a b = if a=42 then f7 (a-1) [a]@b
	else if a<42 then f7 (a*a) b@[a]
	else a::b

let rec f8 = function ([],[]) -> 0
	| ([],y::ys) -> 1+f8 ([],ys)
	| (x::xs,y) -> 1+f8 (xs,y)

let rec f9 x = if f4 x = (-1) then f9 [] else x

let rec f10 i = if i = 0 then 0 else f10 0 + f10 10

let rec f11 i = if i = 0 then 0 else f11 0 + f11 (i-1)

(* Now take another look at this exercise sheet 7 from winter term 2016/17. You may skip exercise 7.5 *)
