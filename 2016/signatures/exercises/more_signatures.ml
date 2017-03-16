(* By Daniel Schubert *)
(* Typenraten: Was ist der Typ von Aufgabe<n>? *)
(* Die Aufgaben sind in Schwierigkeitsgrade eingeteilt*)

val fold_l : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
val map : ('a -> 'b) -> 'a list -> 'b list
val filter : ('a -> bool) -> 'a list -> 'a list

(*0*)
let a a b = a + b
val aufgabe1 = a 1

(*1*)
let d x y = x y
let e = (+) 1
let aufgabe2 = d e

(*2*)
let aufgabe3 = filter (fun x -> x > 5)
let aufgabe4 = map (fun x -> x > 5)

let i x = x
let aufgabe5 = map i

(*3*)
let aufgabe6 = map a

(*4*)
let b x y = y x
let aufgabe7 = fold_l b

let c x = x 5
let aufgabe8 = filter c

let aufgabe9 = map c








