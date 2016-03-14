let todo _ = failwith "TODO: something is not implemented"

type 'a t = todo
val create : ('a -> 'b) -> 'a -> 'b t
val get : 'a t -> 'a
