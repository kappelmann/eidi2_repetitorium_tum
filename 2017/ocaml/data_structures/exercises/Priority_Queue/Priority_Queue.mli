module type PriorityQueue = sig
	type priority
	type value
	type queue
	(* Gibt eine leere Queue zurück *)
	val empty : queue
	(* True, falls der erste Parameter größer als der zweite ist. *)
	val is_smaller : priority -> priority -> bool
	(* Fügt in die Queue den Wert mit der übergebenen Priorität ein. *)
	val insert : queue -> value -> priority -> queue
	(* Führt ein Update der Priorität für den übergebenen Wert durch.
	   Gibt die neue Queue zurück. Falls der übergebene Wert nicht gefunden wurde, 
	   wird der Wert dennoch in die Queue eingefügt. *)
	val update : queue -> value -> priority -> queue
	(* Entfernt das Element mit der kleinsten Priorität aus der Queue. *)
	val pop : queue -> (queue*value option)
end
