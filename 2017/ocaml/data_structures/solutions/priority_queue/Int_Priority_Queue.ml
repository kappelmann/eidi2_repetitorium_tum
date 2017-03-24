open Priority_Queue

(* Eine Queue, in der sowohl Werte als auch Prioritäten integer sein sollen. 
   Speichert die Elemente einfach in einer Liste. *)
module IntPriorityQueue : (PriorityQueue with type priority=int and type value=int) = struct
	type priority = int
	type value = int
	type queue = (value*priority) list
	let empty = []
	let is_smaller p1 p2 = p1<p2
	let rec insert q v p = match q with [] -> [(v,p)]
		| (v',p')::xs when is_smaller p p' -> (v,p)::(v',p')::xs
		| x::xs -> x::insert xs v p
	let update q v p = insert (List.remove_assoc v q) v p
	let pop = function [] -> ([],None)
		| (v,p)::xs -> (xs,Some v)
end

(* Sortiert die übergebene int Liste aufsteigend mit Hilfe der IntPriorityQueue *)
let sort l = let q = List.fold_left (fun q v -> IntPriorityQueue.insert q v v) IntPriorityQueue.empty l
	in let rec collect acc q = let (q,v) = IntPriorityQueue.pop q in
		match v with None -> acc
		| Some v -> collect (v::acc) q
	in List.rev (collect [] q)

let _ = print_string "This list should be sorted in ascending order:\n"; List.fold_left (fun a x -> print_int x; print_string " " ; a) 0 (sort [2;5;11;3;6;8;9;4;2])
