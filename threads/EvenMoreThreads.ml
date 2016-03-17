let todo = failwith "Todo not implemented"


open Event
open Thread

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

let zcount = todo

let avg_tzcount = todo









