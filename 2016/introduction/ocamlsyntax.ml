(*If-Abfragen*)
if (<bedingung>) then <statement>
    else if (<bedingung>) then <statement
    else <statement>

(*Ungleichheit testen*)
a <> 5 (*a ist nicht gleich 5*)
a != 5 (*Das geht leider nicht!*)

(*Modulo rechnen*)
9 mod 2 (*gibt 1 zurueck*)

(*Global Definition mit let *)
let x = 7
(*x hat jetzt den Wert 7*)
let z = x+3
(*z hat jetzt den Wert 10, x immer noch 7*)
let x = 5
(*x hat jetzt den Wert 5, z immer noch den Wert 10*)

(*Lokale Definition mit let <name> = <wert> in <expression>*)
let z = let x = 1 in x+1
(*z hat jetzt den Wert 2, x hat immer noch den Wert 5, denn das innere
 * x ist hier jetzt nicht mehr sichtbar*)

(*Funktion werden genau gleich definiert, nur nehmen Funktionen zusätzlich Parameter*)
let f x = x+1
(*f ist jetzt eine Funktion, die einen int Parameter nimmt und ein int zurueckgibt*)
let e = f 3
(*e hat jetzt den Wert 4*)
let f a b = a+b
(*Alternative Schreibweise:*)
let f = fun x -> x+1

(*Mehrere Parameter*)
let add a b = a+b

(*Das geht nicht, da fib in der Definition noch unbekannt ist*)
let fib a = if a=0 then 0 else if a=1 then 1 else fib(a-1)+fib(a-2)
(*Wir muessen die Definition mit rec als rekursiv definieren*)
let rec fib a = if a=0 then 0 else if a=1 then 1 else fib(a-1)+fib(a-2)

(*String-Konkatination mit dem Zirkumflex ^*)
let a = "Kevin"
let b = "OCamlmann"
let c = a ^ " " ^ b ^ "!"

(*Coole Matching Sachen*)
let a = 1::2::3::[]
let b = "Empty list"
(*Direkt auf Tupel matchen*)
match (a,b) with ([],x) -> x
        | (x::xs, _) -> "Nonempty list"
(*when in matches*)
match a with 
        | x when (x mod 2)=0 -> "gerade"
        | _ -> "ungerade"

(*Folgende zwei Sachen machen das selbe*)
let f a = function [] -> 0 | x::xs -> a
let f a x = match x with [] -> 0 | x::xs -> a
(*d.h. function matched direkt auf einen weiteren Parameter*)

(*Man kann auch Operatoren definieren!*)
let (==>) a b = a mod b
(*Jetzt kann man schreiben:*)
a ==> b (*Rechnet a mod b*)

(*Inline Funktionen*)
(fun x -> x+1)
(*Anwendung:*)
let a f x = f x
a (fun x -> x*2) 4 (*Gibt als Ergebnis 8*)

(*Was passiert hier denn?!*)
let f x = let x = 1 in x
(*Welchen Wert hat wohl b? *)
let b = f 0
(*Antwort: 1*)

(*fold left und right*)
let mystring = ["Hallo";"du";"EIDI2";"Student"] 
let ergebnis = fold_left (fun a s -> a^" "^s) "" mystring
(*Ergebnis: "Hallo du EIDI2 Student"*)

let ergebnis = fold_right (fun s a -> a^" "^s) "" mystring
(*Ergebnis: "Student EIDI2 du Hallo"*)
let ergebnis = fold_right (fun s a -> s^" "^a) "" mystring
(*Ergebnis: "Hallo du EIDI2 Student" *)

(*ChuChu Zug und Datentypen*)
type farbe = Gruen | Rot | Blau | Lilablassblau

type 'a option = Some of 'a | None

(*Beispiel für einen Zug*)
type 'a train = Lok of ('a * 'a train) | Waggon of ('a * 'a train) | Ende

let chuchu = Lok (1,Waggon(2,Ende))
