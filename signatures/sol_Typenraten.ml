
(* Typenraten: Was ist der Typ von AufgabeN? *)
(* Die Aufgaben sind in Schwierigkeitsgrade eingeteilt*)

(*0*)
let asdf a b = a + b
val aufgabe1 = asdf 1  (* int -> int *)

(*1*)
let dings x y = x y  (* ('a -> 'b) -> 'a -> 'b *)
let diesdas = (+) 1 (* int -> int *)
let aufgabe2 = dings diesdas  (* int -> int *)

(*2*)
let aufgabe3 = filter (fun x -> x > 5) (* int list -> int list *)
let aufgabe4 = map (fun x -> x > 5)  (* int list -> bool list *)

let nochmehr x = x  (* 'a -> 'a *)
let aufgabe5 = map nochmehr  (* 'a list -> 'a list *)

(*3*)
let aufgabe6 = map asdf   (*  int list -> (int -> int) list  *)

(*4*)
val fold_l : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
let blubb x y = y x  (* 'a -> ('a -> 'b) -> 'b *)
let aufgabe7 = fold_l blubb  (* 'a -> ('a -> 'a) list -> 'a *)

val filter : ('a -> bool) -> 'a list -> 'a list
let bla x = x 5  (* (int -> 'a) -> 'a *)
let aufgabe8 = filter bla  (* (int -> bool) list -> (int -> bool) list *)

val map : ('a -> 'b) -> 'a list -> 'b list
let aufgabe9 = map bla  (* (int -> 'a) list -> 'a list *)








