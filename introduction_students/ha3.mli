val ( % ) : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
val id : 'a -> 'a
val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
val neg : ('a -> bool) -> 'a -> bool
module Tuple2 :
  sig
    val fst : 'a * 'b -> 'a
    val snd : 'a * 'b -> 'b
    val map1 : ('a -> 'b) -> 'a * 'c -> 'b * 'c
    val map2 : ('a -> 'b) -> 'c * 'a -> 'c * 'b
  end
module MyList :
  sig
    val cons : 'a -> 'a list -> 'a list
    val length : 'a list -> int
    val map : ('a -> 'b) -> 'a list -> 'b list
    val filter : ('a -> bool) -> 'a list -> 'a list
    val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
    val fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
    val at : int -> 'a list -> 'a option
    val flatten : 'a list list -> 'a list
    val make : int -> 'a -> 'a list
    val range : int -> int -> int list
    val init : int -> (int -> 'a) -> 'a list
    val reduce : ('a -> 'a -> 'a) -> 'a list -> 'a option
    val max_el : 'a list -> 'a option
    val min_max : 'a list -> ('a * 'a) option
    val mem : 'a -> 'a list -> bool
    val find : ('a -> bool) -> 'a list -> 'a option
    val filter_map : ('a -> 'b option) -> 'a list -> 'b list
    val partition : ('a -> bool) -> 'a list -> 'a list * 'a list
    val index_of : 'a -> 'a list -> int option
    val split_at : int -> 'a list -> 'a list * 'a list
    val remove_all : ('a -> bool) -> 'a list -> 'a list
    val remove_first : ('a -> bool) -> 'a list -> 'a list
    val take : int -> 'a list -> 'a list
    val drop : int -> 'a list -> 'a list
    val interleave : 'a -> 'a list -> 'a list
    val split : ('a * 'b) list -> 'a list * 'b list
    val combine : 'a list -> 'b list -> ('a * 'b) list option
  end
module MySet :
  sig
    type 'a t
    val from_list : 'a list -> 'a t
    val to_list : 'a t -> 'a list
    val union : 'a t -> 'a t -> 'a t
    val inter : 'a t -> 'a t -> 'a t
    val diff : 'a t -> 'a t -> 'a t
  end
