open Collection

module ColUtils (C: Collection) : sig
        val filter : ('a -> bool) -> 'a C.t -> 'a C.t
	(* apply function to every element of C *)
        val for_all : ('a -> 'b) -> 'a C.t -> 'b C.t
        val length : 'a C.t -> int
	(* check if an element of C satifies the predicate *)
        val exists : ('a -> bool) -> 'a C.t -> 'a option
        (*merge two collections into one*)
        val merge : 'a C.t -> 'a C.t -> 'a C.t
        (*Make a new collection of tuples from the two given collections e.g. [1;2;3] [a;b;c] 
         * would give [(1,a);(2,b);(3,c)] for a list collection. If the collections have a different size, throw a Different_Length exception*)
        exception Different_Length
        val tuple : 'a C.t -> 'b C.t -> ('a * 'b) C.t
        (*Pull out every nth element of the collection. A call with 0 as a second parameter returns C.empty*)
        val every_nth : 'a C.t -> int -> 'a C.t
end
