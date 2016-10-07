module type Collection = sig
        type 'a t 
        val empty : 'a t
        val add : 'a -> 'a t -> 'a t
        val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
end
