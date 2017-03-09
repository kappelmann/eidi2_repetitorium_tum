(* By Daniel Schubert *)
(* BigStep: Beweise die Aussage *)
(* Die Aufgaben sind in Schwierigkeitsgrade eingeteilt*)

let f = fun x -> 2*x
let g = fun x -> x-1
let double = f
let diff = fun x -> fun y -> x-y
let rec map = fun f -> fun x -> match x with
    | [] -> []
    | x::xs -> f x :: map f xs
let rec init = fun x -> match x with
    | 0 -> []
    | x -> x :: init (x-1)

(* 0 *)
4+5 => 9
f 4 => 8

(* 1 *)
f (g 2) => 2
diff 3 2 => 1

(* 2 *)
diff (f 5) 5 => 5
let x = 5 in g (f x) => 9
let fa = fun a -> a+1 in fa 2 => 3

(* 3 *)
diff (f 5) (double (g 2)) => 8
let fb = fun x -> fun y -> x + y in fb (fb 5 2) 8 => 15
let x = 3 in let x = fun x -> x in x 2 => 2
init 3 => [3;2;1]

(* 6 *)
map double (init 3) => [6;4;2]
