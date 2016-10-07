(* By Daniel Schubert *)
(* Typenraten: Was ist der Typ von Aufgabe<n>? *)
(* Die Aufgaben sind in Schwierigkeitsgrade eingeteilt*)

val fold_l : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
val map : ('a -> 'b) -> 'a list -> 'b list
val filter : ('a -> bool) -> 'a list -> 'a list

(*0*)
let a a b = a + b (* int -> int -> int *)
val aufgabe1 = a 1  (* int -> int *)

(*1*)
let d x y = x y  (* ('a -> 'b) -> 'a -> 'b *)
let e = (+) 1 (* int -> int *)
let aufgabe2 = d e  (* int -> int *)

(*2*)
let aufgabe3 = filter (fun x -> x > 5) (* int list -> int list *)
let aufgabe4 = map (fun x -> x > 5)  (* int list -> bool list *)

let i x = x  (* 'a -> 'a *)
let aufgabe5 = map i  (* 'a list -> 'a list *)

(*3*)
let aufgabe6 = map a   (* int list -> (int -> int) list *)

(*4*)
let b x y = y x  (* 'a -> ('a -> 'b) -> 'b *)
let aufgabe7 = fold_l b  (* 'a -> ('a -> 'a) list -> 'a *)

let c x = x 5  (* (int -> 'a) -> 'a *)
let aufgabe8 = filter c  (* (int -> bool) list -> (int -> bool) list *)

let aufgabe9 = map c  (* (int -> 'a) list -> 'a list *)








