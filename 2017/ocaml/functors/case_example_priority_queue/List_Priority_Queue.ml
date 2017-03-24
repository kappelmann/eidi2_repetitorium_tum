open Comparable

(* Integers are comparable *)
module IntCompare : Comparable with type value = int = struct
        type value = int
        let is_smaller = (<)
        let is_equal = (=)
end

(* Booleans are also (somehow) comparable *)
module BoolCompare : Comparable with type value = bool = struct
        type value = bool
        let is_smaller a b = b
        let is_equal = (=) 
end

open Priority_Queue

(* Now we implement a priority queue using a comparable module as a parameter.
   We want to use the value of the comparable module as a priority key and
   the is_smaller function in order to find the insertion position 
   of a given priority. *)
module ListPriorityQueue (C : Comparable) : PriorityQueue with type priority := C.value = struct
        type 'a value = 'a
	type 'a queue = ('a value*C.value) list
	let empty = []
	let rec insert q v p = match q with [] -> [(v,p)]
		| (v',p')::xs when C.is_smaller p p' -> (v,p)::(v',p')::xs
		| x::xs -> x::insert xs v p
	let update q v p = insert (List.remove_assoc v q) v p
	let pop = function [] -> ([],None)
		| (v,p)::xs -> (xs,Some v)
end

(* We can now create arbitrary Queues using different priorities with ease,
   as opposed to our old implementation where we would have needed to re-implement the
   whole module. *)
module IntPriorityQueue = ListPriorityQueue(IntCompare)
module BoolPriorityQueue = BoolPriorityQueue(BoolCompare)

(* Sortiert die übergebene int Liste aufsteigend mit Hilfe der IntPriorityQueue *)
let sort l = let q = List.fold_left (fun q v -> IntPriorityQueue.insert q v v) IntPriorityQueue.empty l
	in let rec collect acc q = let (q,v) = IntPriorityQueue.pop q in
		match v with None -> acc
		| Some v -> collect (v::acc) q
	in List.rev (collect [] q)

let _ = print_string "This list should be sorted in ascending order:\n"; List.fold_left (fun a x -> print_int x; print_string " " ; a) 0 (sort [2;5;11;3;6;8;9;4;2])
