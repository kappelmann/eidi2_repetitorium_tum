open Ex4_base

module Lift (B: Base) = struct
        type 'a t = B.t
        let empty = B.empty
        let insert = B.insert
        let fold = B.fold
        let iter f a = fold (fun a b -> f a) a ()
        let map f a = fold (fun a b -> insert (f a) b) a empty
        let filter p a = fold (fun a b -> if p a then insert a b else b) a empty
        (* {a,b} {c,d} => {a,b,c,d}*)
        let append a b = fold (fun a b -> insert a b) a b
        (* Was wollen wir machen: {{a , b},{c, d }}  => {a,b,c,d}*)
        (* Anfangsakkumulator: {}*)
        (* Erster Schritt im fold {a,b} {} => {a,b}*)
        (* Zweiter Schritt im fold {c,d} {a,b} => {a,b,c,d}*)
        let flatten a = fold (fun a b -> append a b) a empty
        let to_list a = fold (fun a b -> a::b) a []
        let of_list l = let rec doit a = function [] -> a
                | x::xs -> doit (insert x a) xs
                in doit empty l
        (*Alternativ*)
        let of_list l = List.fold_right 
                (fun a b -> insert a b) l empty
end

module List = Lift(
        struct 
        type 'a t = 'a list
        let empty = []
        let insert a xs = a::xs
        let rec fold f x a = match x with [] -> a
                | x::xs -> fold f xs (f x a)
        end)

module SearchTree = Lift(
        struct 
        type 'a t = Empty | Node of ('a * 'a t * 'a t)
        let empty = Empty
        let rec insert a = function Empty -> Node (a,Empty,Empty)
                | Node(v,l,r) -> if a<v then Node(v,insert a l,r)
                        else if a>v then Node(v,l,insert a r)
                        else Node(v,l,r)
        let rec fold f x a = match x with Empty -> a
                | Node(v,l,r) -> fold f r (fold f l (f v a))
        end)
