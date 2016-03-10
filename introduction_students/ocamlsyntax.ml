(*If-Abfragen*)
if (<bedingung>) then <statement>
    else if (<bedingung>) then <statement
    else <statement>

(*Ungleichheit testen*)
a <> 5 (*a ist nicht gleich 5*)
a != 5 (*Das geht leider nicht!*)

(*Modulo rechnen*)
9 mod 2 = 1

(*String-Konkatination*)
let a = "Kevin"
let b = "OCamlmann"
let c = a ^ " " ^ b ^ "!"

(*Coole Matching Sachen*)
(*Direkt auf Tupel matchen*)
let a = 1::2::3::[]
let b = "Empty list"
match (a,b) with ([],x) -> x
        | (x::xs, _) -> "Nonempty list"

match a with 
        | x when (x mod 2)=0 -> "gerade"
        | _ -> "ungerade"

(*Man kann auch Operatoren definieren!*)
let (==>) a b = a mod b
(*Jetzt kann man schreiben:*)
a ==> b (*Rechnet a mod b*)

(*Was passiert hier denn?!*)
let f x = let x = 1 in x
(*Welchen Wert hat wohl b? *)
let b = f 0
(*Antwort: *)


