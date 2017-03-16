open Event

(* Threaded map: Wertet für jedes Element die übergebene Funktion
   in einen eigenen Thread parallel aus. *)
val tmap : ('a -> 'b) -> 'a list -> 'b list

(*in den integer sind immer die belegten Plätze angegeben*)
type w = Waggon of int * w | End
type z = Zug of int * w

(* Zählt die belegten Plätze in einem Zug (ohne Threads).
   Implementieren Sie die Funktion endrekursiv! *)
val zcount : z -> int

(* Berechnet die durchschnittliche Anzahl an belegten Plätzen. 
   Die Plätze in jedem Zug sollen parallel gezählt werden *)
val avg_tzcount : z list -> float

(* Sucht parallel in einer Liste von Listen nach einem Element, das
   das übergebene Prädikat erfüllt.
   Beispiel: tfind (a mod 2 =0) [[1;3;5];[(-1);3;11;8;11]] = Some 8
             tfind (a mod 2 =0) [[1;3;5];[(-1);3;11;11]] = None *)
val tfind : ('a -> bool) -> 'a list list -> 'a option

(* Parallele Berechnung der i-ten Fibonacci-Zahl
   tfib 0 = 0
   tfib 1 = 1
   tfib n = (tfib (n-1)) + (tfib (n-2))
   Wobei de rekursive Berechnung immer parallelisiert in zwei neuen Threads passieren soll*)
val tfib : int -> int

(* Gibt den Wert des ``schneller reagierenden'' Events zurück. *)
val ch_faster : 'a event -> 'a event -> 'a

(* Gibt den Wert des ``langsamer reagierenden'' Events zurück. *)
val ch_slower : 'a event -> 'a event -> 'a

(* Gibt den Wert beider Events als Tupel zurück. Achten Sie darauf, dass ein schnelleres Event 
   nicht auf ein langsameres warten muss. *)
val ch_both : 'a event -> 'b event -> 'a * 'b

(* Gibt den Wert des zuletzt eintretenden Events zurück. *)
val ch_slowest : 'a event list -> 'a option

(* Wendet parallel die i-te Funktion auf das i-te Element der übergebenen
   Listen an und gibt das Ergebnis des am schnellst terminierendsten Funktionsaufrufs zurück.
   Bei unterschiedlichen Listenlängen oder leeren Listen wird None zurückgegeben.
   Beispiel: tcalc [f;g;h;i] [a;b;c;d] berechnet parallel
   f a und g b und h c und i d und gibt davon das am schnellst reagierendste Ergebnis
   zurück *)
val tcalc : ('a -> 'b) list -> 'a list -> 'b option

type s = Send of string
type r = Fetch | Receive of string

(* Startet einen Thread für eine Mailbox, welche Nachrichten in einen Stack verwaltet.
   Gibt zwei Channel zurück, welche jeweils Send oder Fetch operationen annehmen. 
   Wenn ein Send empfangen wird, wird die empfangene Nachricht auf den Stack gelegt.
   Wenn ein Fetch empfangen wird, wird die erste Nachricht vom Stack genommen und mit über ein
   Receive zurückgesendet.
   Wenn keine Nachrichten auf dem Stack liegen, soll ``empty'' als Nachricht zurückgeschickt werden. *)
val start_box : unit -> (s channel * r channel)
