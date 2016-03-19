open Event

(* Threaded map: Wertet für jedes Element die übergebene Funktion
 * in einen eigenen Thread parallel aus.*)
val tmap : ('a -> 'b) -> 'a list -> 'b list

(* Gibt den Wert des zuletzt eintretenden Events zurück.*)
val select_last : 'a event list -> 'a

(*in den integer sind immer die belegten Plätze angegeben*)
type w = Waggon of int * w | End
type z = Zug of int * w
(*Zählt die belegten Plätze in einem Zug (ohne Threads)*)
val zcount : z -> int
(*Berechnet die durchschnittliche Anzahl an belegten Plätzen. 
 *Die Plätze in jedem Zug sollen parallel gezählt werden*)
val avg_tzcount : z list -> float

(*Sucht parallel in einer Liste von Listen nach einem Element, das
 *das übergebene Prädikat erfüllt.
 *Beispiel: tfind (a mod 2 =0) [[1;3;5];[(-1);3;11;8;11]] = Some 8
 *          tfind (a mod 2 =0) [[1;3;5];[(-1);3;11;11]] = None *)
val tfind : ('a -> bool) -> 'a list list -> 'a option

(*Wendet parallel die i-te Funktion auf das i-te Element der übergebenen
 *Listen an und gibt das Ergebnis des am schnellst terminierendsten Funktions=
 *aufrufs zurück. Bei unterschiedlichen Listenlängen oder leeren Listen wird None
 *zurückgegeben.
 *Beispiel: tcalc [f;g;h;i] [a;b;c;d] berechnet parallel
 *f a und g b und h c und i d und gibt davon das am schnellst reagierendste Ergebnis
 *zurück*)
val tcalc : ('a -> 'b) list -> 'a list -> 'b option

(*Parallele Berechnung der i-ten Fibonacci-Zahl
 *tfib 0 = 0
 *tfib 1 = 1
 *tfib n = (tfib (n-1)) + (tfib (n-2))
 *Wobei de rekursive Berechnung immer parallelisiert in zwei neuen Threads passieren soll*)
val tfib : int -> int
