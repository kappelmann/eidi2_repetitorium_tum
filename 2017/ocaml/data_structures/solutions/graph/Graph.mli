(* Ein Graph, dessen Knoten mit Integern gespeichert werden sollen *)
module type Graph = sig
        type node = int
        type edge
	type graph
	(* Gibt einen leeren Graphen zurück *)
	val empty : graph
        (* Erstellt eine Kante aus den beiden Knoten (1. Knoten ``from'', 2. Knoten ``to'') *)
        val edge : node -> node -> edge
        (* Überprüft, ob ein Knoten bereits im Graphen vorhanden ist *)
        val has_node : graph -> node -> bool
        (* Überprüft, ob eine Kante bereits im Graphen vorhanden ist *)
        val has_edge : graph -> edge -> bool
        (* Gibt die Nachbarn eines Knotens zurück *)
        val neighbours : graph -> node -> node list option
        (* Fügt einen neuen Knoten ein, falls noch nicht vorhanden *)
        val insert_node : graph -> node -> graph
        (* Fügt eine neue Kante in den Graph ein, falls noch nicht vorhanden. 
         * Falls einer der beiden Knoten noch nicht vorhanden ist, soll dieser in den
         * Graphen eingefügt werden. *)
        val insert_edge : graph -> edge -> graph
        (* Entfernt einen Knoten aus dem Graphen *)
        val remove_node : graph -> node -> graph
        (* Entfernt eine Kante aus dem Graphen *)
        val remove_edge : graph -> edge -> graph
end
