open Comparable

(* We improve our old priority queue implementation from 2017/data_structures/examples/priority_queue by
   adding the possibility to store arbitrary values *)
module type PriorityQueue = sig
	type priority
	type 'a value = 'a
	type 'a queue
	(* Gibt eine leere Queue zurück *)
	val empty : 'a queue
	(* Fügt in die Queue den Wert mit der übergebenen Priorität ein. *)
	val insert : 'a queue -> 'a value -> priority -> 'a queue
	(* Führt ein Update der Priorität für den übergebenen Wert durch.
	   Gibt die neue Queue zurück. Falls der übergebene Wert nicht gefunden wurde, 
	   wird der Wert dennoch in die Queue eingefügt. *)
	val update : 'a queue -> 'a value -> priority -> 'a queue
	(* Entfernt das Element mit der kleinsten Priorität aus der Queue. *)
	val pop : 'a queue -> ('a queue* 'a value option)
end
