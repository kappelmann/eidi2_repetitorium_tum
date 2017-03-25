let todo _ = failwith "Todo not implemented"

open Graph 

module GraphImpl : (Graph with type node = int and type graph = (int*int list) list) = struct
	type node = int
	(* Speicher den Graph mit Adjazenzlisten *)
	type graph = (node*node list) list
	let rec successors g n = match g with [] -> []
		| (x,xl)::xs when x=n -> xl
		| _::xs -> successors xs n
end

module DFSSearch (G: Graph) = struct
(* Lösung bleibt leider unveröffentlicht. *)
	let fold g from f i = todo ()
end

module BFSSearch (G: Graph) = struct
	let fold g from f i = let rec aux (seen,acc) =
		function [] -> (seen,acc)
		| from::fs ->  if List.exists ((=) from) seen then (seen,acc)
			else aux (from::seen,f acc from) (fs@(G.successors g from))
		in snd @@ aux ([],i) [from]
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
