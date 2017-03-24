let todo _ = failwith "Todo not implemented"

open Graph

module GraphImpl : (Graph with type node = todo and type graph = todo) = struct
        type node
        type graph 
        let rec successors = todo ()
end

(* Implementiere eine Tiefensuche. *)
module DFSSearch (G: Graph) = struct
	(* Folde über den Baum per Tiefensuche. 
	   Parameter:
		g: Graph über den gefoldet wird.
		from: Startknoten
		f: Funktion, mit der gefoldet wird. Übernimmt als Parameter den Akkumulator und den aktuellen Knoten
		i: Startwert des Akkumulators
	   Rückgabewert: Endwert des Akkumulators *)
        let fold g from f i = todo ()
end

(* Implementiere eine Breitensuche. *)
module BFSSearch (G: Graph) = struct
	(* Signatur Vgl. DFSSearch.fold *)
	let fold = todo ()
end

(* Tests *)
let g = [(1,[2;3;4]);(2,[5]);(3,[1]);(4,[]);(5,[])]

module DFSTest = DFSSearch(GraphImpl)
let _ = print_string (if DFSTest.fold g 1 (fun a x -> x) 0 = 4 then 
	"Your dfs seems to work\n" else "Your dfs does not seem to work\n")

let _ = print_string (if DFSTest.fold g 1 (fun a x -> a+x) 0 = 15 then 
	"Your dfs does not visit duplicates\n" else "Your dfs may not visit every node exactly once.\n")

module BFSTest = BFSSearch(GraphImpl)
let _ = print_string (if BFSTest.fold g 1 (fun a x -> x) 0 = 5 then 
	"Your bfs seems to work\n" else "Your bfs does not seem to work\n")

let _ = print_string (if BFSTest.fold g 1 (fun a x -> a+x) 0 = 15 then 
	"Your bfs does not visit duplicates\n" else "Your bfs may not visit every node exactly once.\n")
