let rec app l1 l2 = match l1 with [] -> l2
        | x::xs -> x::app xs l2
(*Zeige: app l1 l2 terminiert für alle l1, l2*)

(* Zeige: random b l terminiert für alle
 * boolschen Werte b und alle Listen l
 * Machen wir eventuell noch später im Rep.*)
let rec random b l = match b with
        | true -> match l with [] -> 0 
                | x::xs -> random (not b) xs; ;
        | false -> match l with [] -> 1
                | x::xs -> random (not b) xs; ;
