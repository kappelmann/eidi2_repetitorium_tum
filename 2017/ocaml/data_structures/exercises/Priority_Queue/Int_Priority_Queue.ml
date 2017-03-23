let todo _ = failwith "Something is not implemented"

open Priority_Queue

(* Eine Queue, in der sowohl Werte als auch Prioritäten integer sein sollen. 
   Speichert die Elemente einfach in einer Liste. *)
module IntPriorityQueue : PriorityQueue = struct
	type priority
	type value
	type queue
        let empty = todo ()
	let is_smaller = todo ()
	let rec insert q v p = todo ()
	let update q v p = todo ()
	let pop = todo ()
end

(* Sortiert die übergeben int Liste mit einer Priority-Queue *)
let sort l q = todo ()

let _ = print_string "This list should be sorted in ascending order:\n"; List.fold_left (fun a x -> print_int x; print_string " " ; a) 0 (sort [2;5;11;3;6;8;9;4;2])
