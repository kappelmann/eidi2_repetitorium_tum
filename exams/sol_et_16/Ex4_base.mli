module type Base = sig
        type 'a t
        val empty : 'a t
        val insert : 'a -> 'a t -> 'a t
        val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
end
