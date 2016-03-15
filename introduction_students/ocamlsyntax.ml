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
(*Antwort: 1*)

(*fold left und right*)
let mystring = ["Hallo";"du";"EIDI2";"Student"] 
let ergebnis = fold_left (fun a s -> a^" "^s) "" mystring
let ergebnis = "Hallo du EIDI2 Student"

let ergebnis = fold_right (fun s a -> a^" "^s) "" mystring
let ergebnis = "Student EIDI2 du Hallo"
let ergebnis = fold_right (fun s a -> s^" "^a) "" mystring
let ergebnis = "Hallo du EIDI2 Student"

(*ChuChu Zug und Datentypen*)
type farbe = Gruen | Rot | Blau | Lilablassblau

type 'a option = Some of 'a | None

(*Beispiel für einen Zug*)
type 'a train = Lok of ('a * 'a train) | Waggon of ('a * 'a train) | Ende

let chuchu = Lok (1,Waggon(2,Ende))

