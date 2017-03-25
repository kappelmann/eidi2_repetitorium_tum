let todo _ = failwith "Something is not implemented"

open Graph

module Graph_Adjacency_List : Graph = struct
        type node = int
        type edge = {node1:node;node2:node}
	type graph = (node * node list) list
        let empty = []
        let edge n1 n2 = {node1=n1;node2=n2}
        let has_node g n = List.exists (fun (n',l) -> n'=n) g
        let has_edge g {node1=n1;node2=n2} = List.exists (fun (n',l) -> n'=n1 && List.exists (fun x -> x=n2) l) g
        let neighbours g n = try Some (List.assoc n g) with Not_found -> None
        let insert_node g n = if has_node g n then g else (n,[])::g
        let insert_edge g {node1=n1;node2=n2} = 
               (* Hilfsfunktion, um Kante zu n in l nur einzufügen, falls noch nicht vorhanden *)
               let check_insert l n = if List.exists (fun n' -> n'=n) l then l else n::l in
               (* Füge zuerst die beiden Knoten ein, falls noch nicht vorhanden *)
               let g = insert_node (insert_node g n1) n2 in
               List.fold_left (fun g (n,l) -> if n=n1 then (n,check_insert l n2)::g
                               else if n=n2 then (n,check_insert l n1)::g
                               else (n,l)::g) empty g 
        let remove_node g n = List.fold_left (fun g (n',l) -> if n'=n then g
                                             else (n',List.filter (fun x -> x<>n) l)::g) empty g
        let remove_edge g {node1=n1;node2=n2} = 
                let check_remove n l = if n=n1 then List.filter (fun x -> x<>n2) l
                        else if n=n2 then List.filter (fun x -> x<>n1) l else l
                in List.fold_left (fun g (n',l) -> (n',check_remove n' l)::g) empty g
end
