open Tree

(*A binary tree that only stores information in its inner nodes. Leafs are empty.
 * The key and the value are equal.*)
module BinTree : Tree = struct
        type 'a t = Node of('a * 'a t * 'a t) | Leaf
        let empty = Leaf
        let to_list t = let rec create t xs = match t with Leaf -> xs
                | Node (v,l,r) -> create l (v::xs) |> create r
                in create t []
                (*let rec to_list t = match t with Leaf -> []
                  	| Node(v,l,r) -> to_list l @ [v] @ to_list r*)
	(*Duplicate values should not be inserted!*)
        let rec add p t = match t with Leaf -> Node(p,Leaf,Leaf)
                | Node(v,l,r) -> if(v=p) then t else if p<v then Node(v,add p l,r)
                        else Node(v,l,add p r)
        let rec fold f a = function Leaf -> a
                | Node(v,l,r) -> fold f (fold f (f a v) l) r
        let merge a b = fold (fun t a -> add a t) a b
        let rec min = function Leaf -> raise (Failure "Empty tree, no min.")
                | Node(v,Leaf,_) -> v
                | Node(_,l,_) -> min l
        let greater_than a n = let rec create a t = match a with Leaf -> t
                | Node(v,l,r) -> if v>n then add v t |> create l |> create r
                                 else create r t
                in create a empty
        (*Alternative mit fold*)
        let greater_than t n = fold (fun acc elem -> if elem>n then 
                        add elem acc else acc) empty t
end
