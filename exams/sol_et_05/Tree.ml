type 'a tree = Tree of('a * 'a tree list)
let rec size = function Tree (_,[]) -> 1
        | Tree (v, x::xs) -> (size x) + (size (Tree(v,xs)))

let rec map f Tree(v,l) = 
        let rec sub = function [] -> []
        | x::xs -> map f x::sub xs in
        Tree(f v, sub l)

let rec to_list t = 
        let rec sub l a = match l with [] -> a
        | x::xs -> acc a x |> sub xs 
        and acc a = function Tree(v,l) -> sub l (v::a)
        in acc [] t
