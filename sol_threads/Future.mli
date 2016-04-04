open Event

type 'a t = 'a channel
val create : ('a -> 'b) -> 'a -> 'b t
val get : 'a t -> 'a
