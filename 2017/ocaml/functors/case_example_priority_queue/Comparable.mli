module type Comparable = sig
        type value
        val is_smaller : value -> value -> bool
        val is_equal : value -> value -> bool
end
