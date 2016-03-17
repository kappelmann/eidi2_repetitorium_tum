let todo = failwith "Todo not implemented"

open Thread
open Event

let rec map f = function [] -> []
        | x::xs -> f x::map f xs

let tmap f l = let rec help = function [] -> []
        | x::xs -> let c = new_channel ()
          in let _ = create (fun c -> sync(send c (f x))) c 
          in (receive c)::tmap f xs
        in map sync (help l)

let rec length = function [] -> 0
        | _::xs -> 1+length xs

let select_last el = let l = length el in
        let help i = let res = select el in
                if i=l then res else help (i+1)
        in help 1

let zcount (Zug(i',wl)) = match wl with 
        | End -> i'
        | Waggon(i,wl) -> i+zcount (Zug (i',wl))

let rec sum = function [] -> 0
        | x::xs -> x+sum xs

let avg_tzcount l = let zl = length l in
        let el = tmap zcount l in
        (float_of_int(sum el)) /. ((float_of_int)zl)
