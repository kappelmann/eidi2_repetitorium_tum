(* 1. Sie erstellen eine laufzeitkritische Anwendung. Welche der folgenden Definitionen würden sie bevorzugen. Warum? *)
let rec append_1 x y = match x with [] -> y | x::xs -> x::append_1 xs y

let append_2 x y = let rec aux acc = function [] -> acc
        | x::xs -> aux (x::acc) xs
        in aux y (List.rev x)

let append_3 x y = x@y

(* Antwort: append_1 ist nicht endrekursiv und somit für sehr lange Listen ungeeignet (potentieller Stackoverflow). 
            append_2 hingegen ist endrekursiv und somit auch für sehr lange Listen geeignet.
            append_3 verwendet den eingebauten Append-Operator von OCaml, dieser ist allerdings nicht endrekursiv implementiert.
   --> append_2 ist zu bevorzugen. *)

(* 2. Implementieren Sie folgende Funktionen endrekursiv *)
(* Sie dürfen für gauss x>=0 und für h p>=0 annehmen. *)
let rec gauss = function x when x>1 -> gauss (x-1)+x
	| x -> x

let gauss_tail n = let rec gauss_aux acc n' = if n=n' then n+acc
	else gauss_aux (acc+n') (n'+1)
	in gauss_aux 0 0

(* Oder auch ganz ausgefuchst *)
let gauss_tail n = (n*(n+1))/2

let rec h e l p = match p with 0 -> (p+1)
	| x -> e*l*h e l (x-1)

let h_tail e l p = let rec h_aux acc p = if p=0 then acc
	else h_aux (acc*e*l) (p-1)
	in h_aux 1 p

let g f l = let rl = List.rev l in 
	List.fold_right f rl 0

let g_tail f l = List.fold_left f 0 l

let rec a x y = function ([],_) -> y | (_,[]) -> x | (x::xs,_) -> x::a xs y

let a_tail = append_2 x y

let fold_right f l acc = List.fold_right f l acc

let fold_right_tail f l acc = List.fold_left f acc (List.rev l)

(* Alternativ mit Continuations *)
let rec a_tail x y = let rec aux = function (cont,[],ys) -> cont ys
        | (cont,x::xs,ys) -> aux ((fun acc -> cont (x::acc)),xs,ys)
        in aux ((fun i -> i), x, y)

(* 3. Sind folgende Funktionen endrekursiv? Begründung! *)

(* Ja, keine Rekursion. *)
let f1 a b = a*b

(* Ja, der letzte Aufruf ist der rekursive Aufruf. Dass das Programm nicht terminiert, ist irrelevant. *)
let rec f2 a b = f2 a b

(* Ja, der letzte Aufruf ist ein Aufruf einer enrekursiven Funktion. *)
let f3 a b = f2 a b

(* Nein, die letzte Operation im rekursiven Fall ist die Negation des rekursiven Aufrufs, somit ist die Funktion
   nicht endrekursiv. *)
let rec f4 = function [] -> 1
	| x::xs -> -f4 xs

(* Ja, beide Funktionen sind endrekursiv. In beiden Funktionen ist nämlich der letzte Aufruf ein Funktionsaufruf
   der jeweils anderen Funktion. Somit sind die beiden Funktionen zusammen endrekursiv. *)
let rec f5 = function [] -> 1
	| x::xs -> f6 xs
and f6 = function [] -> 2
	| x::xs -> f5 xs 

(* Nein, die letzte Operation in den Fällen a<=42 ist jeweils die Anwendung des @-Operators. *)
let rec f7 a b = if a=42 then f7 (a-1) [a]@b
	else if a<42 then f7 (a*a) b@[a]
	else a::b

(* Nein, der letzte Aufurf ist im rekursiven Fall die Addition mit 1 *)
let rec f8 = function ([],[]) -> 0
	| ([],y::ys) -> 1+f8 ([],ys)
	| (x::xs,y) -> 1+f8 (xs,y)

(* Nein, in der If-Abfrage wird die nicht endrekursive Funktion f4 verwendet! Somit ist f9 nicht endrekursiv. *)
let rec f9 x = if f4 x = (-1) then f9 [] else x

(* Ja, die Rekursionstiefe lässt sich mit 2 begrenzen *)
let rec f10 i = if i = 0 then 0 else f10 0 + f10 0

(* Nein, die Rekursionstiefe hängt von dem Parameter i ab und lässt
 * sich somit nicht durch eine Konstante begrenzen *)
let rec f11 i = if i = 0 then 0 else f11 0 + f11 (i-1)


(* Jetzt schaut euch nochmal Übungsblatt 7 dieses Jahres an. Aufgabe 7.5 kann man eventuell überspringen. *)
