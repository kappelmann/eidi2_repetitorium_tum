(* Threaded map: Wertet für jedes Element die übergebene Funktion
 * in einen eigenen Thread parallel aus.*)
val tmap : ('a -> 'b) -> 'a list -> 'b list

(* Gibt den Wert des zuletzt eintretenden Events zurück.*)
val select_last : 'a event list -> 'a

