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
