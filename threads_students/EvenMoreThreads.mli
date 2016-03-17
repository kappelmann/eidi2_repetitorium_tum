(* Threaded map: Wertet für jedes Element die übergebene Funktion
 * in einen eigenen Thread parallel aus.*)
val tmap : ('a -> 'b) -> 'a list -> 'b list


