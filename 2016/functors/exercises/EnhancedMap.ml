let todo _ = failwith "Todo implement this map method"

open SimpleMap

module EnhancedMap (S: SimpleMap) = struct
        include S
        let is_greater = todo ()
        let is_smaller = todo ()
        let is_equal = todo ()
        let map = todo ()
        let map_val = todo ()
        let map_key = todo ()
        let exists = todo ()
        let exists_key = todo ()
        let fold f a m = todo ()
        let remove p m = todo ()
        let remove_key = todo ()
        let count_dupl = todo ()
end
