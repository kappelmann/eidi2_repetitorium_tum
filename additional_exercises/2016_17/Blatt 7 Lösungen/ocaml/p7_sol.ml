

(* Eine unendliche Liste kann durch eine "lazy" Liste abgebildet werden. 
 * Diese besteht dabei aus dem ersten Element (head), sowie einer Funktion,
 * die bei Bedarf den Rest der Liste (tail) erzeugen kann *)
type 'a llist = Cons of 'a * (unit -> 'a llist)

(* 1) Die Funktion val lseq : int -> int llist, die eine unendliche Folge von 
 * ganzen Zahlen erzeugt. Die Folge soll mit der übergebenen Zahl beginnen. *)
let rec lseq start = Cons (start, fun () -> lseq (start+1))

(* 2) Die Funktion val lconst : int -> int llist, die eine unendliche Folge 
 * der übergebenen Zahl erzeugt. *)
let rec lconst n = Cons (n, fun () -> lconst n)

(* 3) Die Funktion val lpowers2 : unit -> int llist, die 2er-Potenzen 
 * (beginnend mit 1) erzeugt. *)
let lpowers2 () = 
    let rec lpowers2_impl start = Cons (start, fun () -> lpowers2_impl (2*start)) in
    lpowers2_impl 1

(* 4) Die Funktion val lrng : int -> int llist, die eine Folge zufälliger Werte
 * erzeugt. Diese Werte sollen dabei zwischen 0 und dem übergebenen Maximalwert
 * (inklusive) liegen. Eine zufällige Ganzzahl aus dem Interval [0, n) lässt sich
 * in OCaml mit der Funktion Random.int n erzeugen. *)
let rec lrng max = Cons (Random.int (max+1), fun () -> lrng max)

(* 5) Die Funktion val lfib : unit -> int llist, welche die Fibonacci-Folge 
 * (beginnend mit 0 und 1) erzeugt.) *)
let lfib () =
    let rec lfib_impl n0 n1 = Cons (n0, fun () -> lfib_impl n1 (n0+n1))
    in lfib_impl 0 1

(* 6) Die Funktion val lhd : 'a llist -> 'a, die das erste Element (Head) der 
 * Liste zurückgibt.*)
let lhd (Cons (head, _)) = head

(* 7) Die Funktion val ltl : 'a llist -> 'a llist, die den Rest der Liste (Tail) 
 * liefert.*)
let ltl (Cons (_, tail)) = tail () 

(* 8) Die Funktion val ltake : int -> 'a llist -> 'a list, die für ein übergebenes
 * n eine endliche Liste mit den ersten n Elementen liefert.*)
let rec ltake n (Cons (head, tail)) =
    if n = 0 then []
    else head :: (ltake (n-1) (tail ()))

(* 9) Die Funktion val ldrop : int -> 'a llist -> 'a llist, die für ein übergebenes
 * n die ersten n Elemente der Liste entfernt.*)
let rec ldrop n (Cons (head, tail) as ll) =
    if n = 0 then ll
    else ldrop (n-1) (tail ())

(* 10) Eine Funktion val lappend : 'a llist -> 'a llist -> 'a llist macht keinen
 * Sinn. Eine mögliche Implementierung wäre wenn dann höchstens, einfach die erste
 * Liste zurückzugeben, da sowieso niemals Elemente der zweiten Liste erreicht würden *)

(* 11) Die Funktion val lnth : int -> 'a llist -> 'a, die das n-te Element der Liste
 * zurückgibt. *) 
let rec nth n (Cons (head, tail)) =
    if n = 0 then head
    else nth (n-1) (tail ())

(* 12) Eine Funktion val lreverse : 'a llist -> 'a llist lässt sich im Allgemeinen
 * nicht realisieren. Selbst wenn die tail-Funktion invertierbar wäre, hätte man 
 * kein "letztes Element", das man als Head der neuen Liste verwenden könnte. *)
 
(* 13) Die Funktion val lfilter : ('a -> bool) -> 'a llist -> 'a llist, die eine 
 * neue Liste zurückgibt, die nur noch diejenigen Elemente enthält, die das übergebene
 * Prädikat erfüllen. *)
let rec lfilter f (Cons (head, tail)) =
    if f head then Cons (head, fun () -> lfilter f (tail ()))
    else lfilter f (tail ())

(* 14) \item Die Funktion val lmap : ('a -> 'b) -> 'a llist -> 'b llist, die alle 
 * Elemente mit Hilfe der übergebenen Funktion transformiert und daraus eine neue  
 * Liste erzeugt. *)
let rec lmap f (Cons (head, tail)) =
    Cons (f head, fun () -> lmap f (tail ()))

(* 15) Die Funktion val linterleave : 'a llist -> 'a llist -> 'a llist, die ein 
 * Interleaving der beiden übergebenen Listen erzeugt. So soll zum Beispiel aus 
 * den beiden Listen [1; 2; 3; 4; 5; 6; ...] und [20; 20; 20; 20; 20; 20; ...]
 * die neue Liste [1; 20; 2; 20; 3; 20; 4; 20; 5; 20; 6; 20; ...] entstehen. *)
let rec linterleave (Cons (head, tail)) l =
    Cons (head, fun () -> linterleave l (tail ()))

(* 16) Die Funktion val linterleaven : 'a llist list -> 'a llist, die als eine 
 * Verallgemeinerung der Funktion linterleave zu verstehen ist und ein Interleaving
 * einer beliebigen Anzahl von Listen erzeugen kann. *)
let rec linterleaven = function
    | [] -> failwith "invalid argument"
    | (Cons (head, tail)) :: t -> Cons (head, fun () -> linterleaven (t @ [tail ()]))

(* 17) Die Funktion val lrepeat : 'a list -> 'a llist, die eine Liste erzeugt, 
 * welche die Elemente der übergebenen (endlichen) Liste zyklisch wiederholt. Aus
 * der Liste [1; 2; 3] wird also die unendliche Folge [1; 2; 3; 1; 2; 3; 1; 2; 3; ...]
 * erzeugt. *)
let rec lrepeat l =
    match l with 
    | [] -> failwith "invalid argument"
    | h :: t -> Cons (h, fun () -> lrepeat (t @ [h]))

(* 18) Die Funktion val lprimes : unit -> int llist, die alle Primzahlen in 
 * aufsteigender Reihenfolge erzeugt. *)
let lprimes () = 
    let rec lprimes_impl (Cons (head, tail)) =
        Cons (head, fun() -> lprimes_impl (lfilter (fun x -> x mod head <> 0) (tail ())))
    in lprimes_impl (lseq 2)

(* 19) Die Funktion val lbinary : unit -> int list llist, die alle Wörter über dem Alphabet
 * { 0, 1 }, also alle Folgen von 0 und 1 erzeugt. Ein Wort wird dabei als (endliche)
 * int list repräsentiert. Das Wort 0010 kommt also irgendwo in der zurückgegebenen 
 * Liste [... [0; 0; 1; 0]; ...] vor. *)
let lbinary () =    
    let rec lbinary_impl l =
        Cons (l, fun () -> linterleave (lbinary_impl (0 :: l)) (lbinary_impl (1 :: l)))
    in lbinary_impl []

(* 20) Betrachten Sie die Funktion val lmagic : unit -> float llist. Was könnte diese
 * Funktion erzeugen? Hinweis: Probieren Sie es aus! *)
let lmagic () =
    let rec lmagic_impl a b =
        let d = float_of_int a in
        let c = b +. ((16. ** (-.d)) *. ((4. /. (8. *. d +. 1.)) -. (2. /. (8. *. d +. 4.)) -.
                (1. /. (8. *. d +. 5.)) -. (1. /. (8. *. d +. 6.)))) in
        Cons ((int_of_float (c *. (10. ** (float_of_int a)))) mod 10, fun () -> lmagic_impl (a+1) c)
    in lmagic_impl 0 0.