open SimpleMap

module IntMap : SimpleMap = struct
        type k = Key of int
        type 'a v = Value of 'a
        type 'a p = Pair of (k * 'a v)
        type 'a t = Map of 'a p list
        let compare (Key k1) (Key k2) = if k1=k2 then 0 else if k1<k2 then (-1) else 1
        let create_p (Key k) (Value v) = Pair((Key k),(Value v))
        let get_val (Pair(k,v)) = v
        let get_key (Pair(k,v)) = k
        let empty = (Map [])
        let insert_p : 'a p -> 'a t -> 'a t = fun p (Map l) -> Map(p::l)
        let insert k v m = insert_p (create_p k v) m
        let rem_first = function (Map []) -> None
                | Map (x::xs) -> Some (x,(Map xs))
end
