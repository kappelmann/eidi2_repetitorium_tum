open Ex4_base

module Lift (B: Base) : sig
        include Base
        val iter : ('a -> unit) -> 'a t -> unit
        val map : ('a -> 'b) -> 'a t -> 'b t
        val filter : ('a -> bool) -> 'a t -> 'a t
        val append : 'a t -> 'a t -> 'a t
        val flatten : 'a t t -> 'a t
        val to_list : 'a t -> 'a list
        val of_list : 'a list -> 'a t
end
