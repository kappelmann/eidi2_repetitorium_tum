open Event

(* Aufgabe 1 *)
let threaded_filter p l c = 
        let rec filter p = function [] -> []
               | x::xs -> if p x then x::filter p xs else filter p xs
        in let _ = Thread.create (fun () -> sync (send c (filter p l))) () 
	in ()

(* Aufgabe 2 *)
type 'a t = 'a channel

let create f v = let c = new_channel() in
        let rec calc v = sync(send c (f v)); calc v in
        let _ = Thread.create calc v in
        c
        
let get c = sync(receive c)

(* Aufgabe 3 *)
type 'a tree = Leaf of 'a | Node of ('a tree * 'a tree)

let rec min t = 
        let open Event in let open Thread in
        let help c t = sync(send c (min t)) in
        match t with Leaf v -> v
        | Node (l,r) -> let (cl,cr) = (new_channel(),new_channel()) in
                        let _ = Thread.create (help cl) l in
                        let _ = Thread.create (help cr) r in
                        let lv = sync(receive cl) in
                        let rv = sync(receive cr) in
                        if lv<rv then lv else rv
