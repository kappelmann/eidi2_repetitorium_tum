open SimpleMap

module EnhancedMap (S: SimpleMap) : sig
        include SimpleMap

        (*Do not use the fold method for the first methods!*)
        val is_greater : k -> k -> bool
        val is_smaller : k -> k -> bool
        val is_equal : k -> k -> bool
        (*Map the function to every pair in the map*)
        val map : ('a p -> 'b p) -> 'a t -> 'b t
        (*Map the function to every value in the map*)
        val map_val : ('a v -> 'b v) -> 'a t -> 'b t
        (*Map the function to every key in the map*)
        val map_key : (k-> k) -> 'a t -> 'a t
        (*Check if a pair fulfilles the predicate*)
        val exists : ('a p -> bool) -> 'a t -> bool
        (*Check if a key exists in the map*)
        val exists_key : (k -> bool) -> 'a t -> bool
        (*No fold up to this point!*)

        (*Okay, lets do it! Here comes the fold!*)
        val fold : ('a -> 'b p -> 'a) -> 'a -> 'b t -> 'a
        (*And now, everything is a fold!*)
        (*Remove any pair that fulfilles the predicate*)
        val remove : ('a p -> bool) -> 'a t -> 'a t
        (*Removes any pair that has the given key*)
        val remove_key : k -> 'a t -> 'a t
        (*Counts the amount of entries for the given key*)
        val count_dupl : k -> 'a t -> int
end

