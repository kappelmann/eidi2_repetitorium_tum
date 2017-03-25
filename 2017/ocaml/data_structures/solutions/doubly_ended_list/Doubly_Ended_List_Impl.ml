let todo _ = failwith "Something is not implemented"

open Doubly_Ended_List

(* Achten Sie bei der Implementierung auf eine möglichst effiziente Laufzeit *)
module DoublyEndedListImpl : DoublyEndedList = struct
        type 'a dlist = {front:'a list; back: 'a list}
        let empty = {front=[];back=[]}
        let push_front l e = {l with front=e::l.front}
	let push_back l e = {l with back=e::l.back}
        let pop_front = function {front=[];back=[]} -> (empty,None)
                | {front=[];back=b} -> let (x::xs) = List.rev b in ({front=xs;back=[]},Some x)
                | {front=x::xs;back=b} -> ({front=xs;back=b},Some x)
	let pop_back = function {front=[];back=[]} -> (empty,None)
                | {front=f;back=[]} -> let (x::xs) = List.rev f in ({front=[];back=xs}, Some x)
                | {front=f;back=x::xs} -> ({front=f;back=xs},Some x)
        let rev l = {front=l.back;back=l.front}
end

open  DoublyEndedListImpl

let l = empty
let rec push op l v = List.fold_left (fun l x -> op l x) l v
let l = push push_front l [5;4;3;2;1]
let l = push push_back l [6;7;8;9;10]

let _ = print_string "If your implementation is correct, you should now see numbers from 1 to 10 and a happy smiley\n"
let l = let rec pf l i = if i=10 then (print_string "\n"; l)
else let (l,Some v) = pop_front l in print_int v; print_string ","; pf l (i+1)
        in pf l 0
let _ = let c = (pop_front l, pop_back l) in if c=((empty,None),(empty,None)) then print_string ":-)\n" else print_string "your pop_front does not work\n"

let l = empty
let rec push op l v = List.fold_left (fun l x -> op l x) l v
let l = push push_front l [5;4;3;2;1]
let l = push push_back l [6;7;8;9;10]

let _ = print_string "If your implementation is correct, you should now see numbers from 10 to 1 and a happy smiley\n"
let l = let rec pf l i = if i=10 then (print_string "\n"; l)
        else let (l,Some v) = pop_back l in print_int v; print_string ","; pf l (i+1)
        in pf l 0
let _ = let c = (pop_front l, pop_back l) in if c=((empty,None),(empty,None)) then print_string ":-)\n" else print_string "your pop does not work\n"
