val todo : 'a -> 'b
val ( % ) : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
val comp2 : ('a -> 'a -> 'b) -> ('c -> 'a) -> 'c -> 'c -> 'b
val compareBy : ('a -> 'b) -> 'a -> 'a -> int
val id : 'a -> 'a
val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
val neg : ('a -> bool) -> 'a -> bool
val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c
val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c
module Tuple2 :
  sig
    val fst : 'a * 'b -> 'a
    val snd : 'a * 'b -> 'b
    val map1 : ('a -> 'b) -> 'a * 'c -> 'b * 'c
    val map2 : ('a -> 'b) -> 'c * 'a -> 'c * 'b
    val map : ('a -> 'b) -> ('c -> 'd) -> 'a * 'c -> 'b * 'd
  end
module Option :
  sig
    val some : 'a -> 'a option
    val is_some : 'a option -> bool
    val is_none : 'a option -> bool
    val get_some : 'a option -> 'a
    val show : ('a -> string) -> 'a option -> string
    val map : ('a -> 'b) -> 'a option -> 'b option
    val bind : 'a option -> ('a -> 'b option) -> 'b option
    val ( >>= ) : 'a option -> ('a -> 'b option) -> 'b option
    val filter : ('a -> bool) -> 'a option -> 'a option
    val default : 'a -> 'a option -> 'a
    val ( |? ) : 'a option -> 'a -> 'a
    val try_some : ('a -> 'b) -> 'a -> 'b option
  end
module List :
  sig
    val cons : 'a -> 'a list -> 'a list
    val head : 'a list -> 'a option
    val tail : 'a list -> 'a list option
    val last : 'a list -> 'a option
    val reverse : 'a list -> 'a list
    val length : 'a list -> int
    val map : ('a -> 'b) -> 'a list -> 'b list
    val iter : ('a -> unit) -> 'a list -> unit
    val mapi : (int -> 'a -> 'b) -> 'a list -> 'b list
    val filter : ('a -> bool) -> 'a list -> 'a list
    val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
    val fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
    val exists : ('a -> bool) -> 'a list -> bool
    val for_all : ('a -> bool) -> 'a list -> bool
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
    val merge : ?cmp:('a -> 'a -> int) -> 'a list -> 'a list -> 'a list
    val sort : ?cmp:('a -> 'a -> int) -> 'a list -> 'a list
  end
module Set :
  sig
    type 'a t = 'a list
    val from_list : 'a list -> 'a t
    val to_list : 'a -> 'a
    val union : 'a list -> 'a list -> 'a t
    val inter : 'a list -> 'a list -> 'a t
    val diff : 'a list -> 'a list -> 'a list
  end
module String :
  sig val concat : string -> string list -> string val escaped : 'a -> 'a end
