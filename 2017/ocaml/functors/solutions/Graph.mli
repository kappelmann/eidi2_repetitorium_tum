(* Modul für einen Graphen. Die Nodes sollen mit integern identifiziert werden. 
   successors g n gibt die Nachbarn von n in g als Liste zurück. *)
module type Graph = sig
        type node
        type graph
        val successors : graph -> node -> node list
end
