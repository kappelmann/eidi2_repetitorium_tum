open Collection

module NativeList : Collection = struct
        type 'a t = 'a list
        let empty = []
        let add p l = match l with 
                | [] -> [p]
                | _ -> p::l
        (*Try to use a tail recursive fold version!*)
        let rec fold f a = function [] -> a
                (*Same thing as fold f (f a x) xs*)
                | x::xs -> (f a x |> fold f) xs
end

module MyList : Collection = struct
        type 'a t = Part of ('a * 'a t) | Tail
        let empty = Tail
        let add p l = match l with 
                | Tail -> Part(p, empty)
                | Part(left,right) -> Part(p,l)
        let rec fold f a = function Tail -> a
                (*Same thing as fold f (f a left) right*)
                | Part(left, right) -> (f a left |> fold f) right
end
