let todo = failwith "Todo not implemented"

open Thread
open Event

let rec map f = function [] -> []
        | x::xs -> f x::map f xs

let rec tmap f l = let rec help = function [] -> []
        | x::xs -> let c = new_channel ()
          in let _ = Thread.create (fun c -> sync(send c (f x))) c 
          in (receive c)::help xs
        in map sync (help l)

let rec length = function [] -> 0
        | _::xs -> 1+length xs

let select_last el = let l = length el in
        let help i = let res = select el in
                if i=l then res else help (i+1)
        in help 1

(*in den integer sind immer die belegten Plätze angegeben*)
type w = Waggon of int * w | End
type z = Zug of int * w

let zcount (Zug(i',wl)) = match wl with 
        | End -> i'
        | Waggon(i,wl) -> i+zcount (Zug (i',wl))

let rec sum = function [] -> 0
        | x::xs -> x+sum xs

let avg_tzcount l = let zl = length l in
        let el = tmap zcount l in
        (float_of_int(sum el)) /. ((float_of_int)zl)

let rec find p = function [] -> None
        | x::xs -> if p x then (Some x) else find p xs

(*Bessere Lösung wäre mit select*)
let tfind p l = let fl = tmap (find p) l in 
        let rec find_some = function [] -> None
                | x::xs -> if x<>None then x else find_some xs
        in find_some fl

let tcalc f l = if (length f)<>(length l) then None
        else if f=[] then None
        else let rec create f l = match (f,l) with
                | ([],[]) -> []
                | (f::fs, x::xs) -> let c = new_channel () in
                        let _ = Thread.create (fun x -> sync(send c (f x))) x
                        in receive c::create fs xs
        in Some (select (create f l))

let rec tfib = function 0 -> 0
        | 1 -> 1
        | n -> let (c1,c2) = (new_channel(),new_channel()) in
               let csend c n = sync(send c (tfib n)) in
               let _ = Thread.create (csend c1) (n-1) in
               let _ = Thread.create (csend c2) (n-2) in
               let sr c = sync(receive c) in
               let (v1,v2) = ((sr c1),(sr c2)) in 
               v1+v2
