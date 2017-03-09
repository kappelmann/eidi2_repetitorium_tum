(*See Future.pdf exercise 13.3*)

type 'a t
val create : ('a -> 'b) -> 'a -> 'b t
val get : 'a t -> 'a
