let cycle = function [] -> []
        | x::xs -> append xs [x]

let ncycle n l = let rec doit i l = if i=n then l else doit (i+1) (cycle l)
        in doit 0 l

let rec bubble = function f::s::xs -> if f<s then f::bubble (s::xs) else s::bubble (f::xs)
        | x -> x

let rec sort l = let rec doit i n l = if i=n then l
        else doit (i+1) n (bubble l)
        in doit 0 (length l) l
