module type DoublyEndedList = sig
	type 'a dlist
	(* Gibt eine leere Liste zurück *)
	val empty : 'a dlist
	(* Fügt in die Liste vorne ein *)
	val push_front : 'a dlist -> 'a -> 'a dlist
	(* Fügt in die Liste hinten ein *)
	val push_back : 'a dlist -> 'a -> 'a dlist
	(* Entfernt das vorderste Element der Liste *)
	val pop_front : 'a dlist -> ('a dlist*'a option)
	(* Entfernt das hinterste Element der Liste *)
	val pop_back : 'a dlist -> ('a dlist*'a option)
        (* Dreht die Liste um *)
        val rev : 'a dlist -> 'a dlist
end
