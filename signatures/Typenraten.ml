
(* Typenraten: Was ist der Typ von AufgabeN? *)
(* Die Aufgaben sind in Schwierigkeitsgrade eingeteilt*)

(*0*)
let asdf a b = a + b
val aufgabe1 = asdf 1

(*1*)
let dings x y = x y
let diesdas = (+) 1
let aufgabe2 = dings diesdas

(*2*)
let aufgabe3 = filter (fun x -> x > 5)
let aufgabe4 = map (fun x -> x > 5)

let nochmehr x = x
let aufgabe5 = map nochmehr

(*3*)
let aufgabe6 = map asdf

(*4*)
val fold_l : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
let blubb x y = y x
let aufgabe7 = fold_l blubb

val filter : ('a -> bool) -> 'a list -> 'a list
let bla x = x 5
let aufgabe8 = filter bla

val map : ('a -> 'b) -> 'a list -> 'b list
let aufgabe9 = map bla








