open SimpleMap

module EnhancedMap (S: SimpleMap) = struct
        include S
        let is_greater k1 k2 = (compare k1 k2)=1
        let is_smaller k1 k2 = (compare k1 k2)=(-1)
        let is_equal k1 k2 = (compare k1 k2)=0
        let map f m = let rec help a m = match rem_first m 
                        with None -> a
                        | Some (e,m) -> help (insert_p (f e) a) m
                in help empty m
        (*Alternativ:*)
        let rec map f m = match rem_first m with None -> empty
                | Some(e,m) -> insert_p (f e) (map f m)
        let map_val f m = map (fun p -> create_p (get_key p) (f (get_val p))) m
        let map_key f m = map (fun p -> create_p (f (get_key p)) (get_val p)) m
        let rec exists p m = match rem_first m 
               with None -> false
               | Some (e,m) -> p e || exists p m
        let exists_key f m = exists (fun p -> f (get_key p)) m
        let fold f a m = let rec help a m = match rem_first m 
                        with None -> a
                        | Some (e,m) -> help (f a e) m
                in help a m
        (*Alternativ:*)
        let rec fold f a m = match rem_first m with None -> a
                | Some(e,m) -> fold f (f a e) m
        let remove p m = fold (fun a x -> if (p x) then a else insert_p x a) empty m
        let remove_key k m = remove (fun x -> is_equal (get_key x) k) m
        let count_dupl k m = fold (fun a x -> if is_equal (get_key x) k then a+1 else a) 0 m
end
