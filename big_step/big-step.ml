let rec app l1 l2 = match l1 with [] -> l2
        | x::xs -> x::app xs l2
(*Zeige: app l1 l2 terminiert für alle l1, l2*)

(* Zeige: random b l terminiert für alle
 * boolschen Werte b und alle Listen l mit dem Wert 1
 * Machen wir eventuell noch später im Rep.*)
let rec random b l = match b with
        | true -> random (not b) l
        | false -> match l with [] -> 1
                | x::xs -> random (not b) xs
