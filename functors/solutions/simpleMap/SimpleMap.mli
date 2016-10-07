(*A simple map that stores key value pairs.
 *If a pair with the same key is inserted twice, it will
 *also be stored twice!*)
module type SimpleMap = sig
        (*Type for keys*)
        type k
        (*Type for values*)
        type 'a v
        (*Type for key,value pairs*)
        type 'a p
        (*Type for the map data structure*)
        type 'a t
        (*Params a and b
         * return -1 if a is smaller than b
         *         0 if a is equal to b
         *         1 if a is greater than b*)
        val compare : k -> k -> int
        val create_p : k -> 'a v -> 'a p
        val get_val : 'a p -> 'a v
        val get_key : 'a p -> k
        val empty : 'a t
        val insert_p : 'a p -> 'a t -> 'a t
        val insert : k -> 'a v -> 'a t -> 'a t
        (*Removes the first element of the map if there is any.
         *Returns a tuple with the removed key value pair and the
         *remaining map*)
        val rem_first : 'a t -> ('a p*'a t) option
end
