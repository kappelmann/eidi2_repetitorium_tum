val todo : 'a -> 'b
val ( % ) : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
val comp2 : ('a -> 'a -> 'b) -> ('c -> 'a) -> 'c -> 'c -> 'b
val compareBy : ('a -> 'b) -> 'a -> 'a -> int
val id : 'a -> 'a
val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
val neg : ('a -> bool) -> 'a -> bool
val const : 'a -> 'b -> 'a
module Tuple2 :
  sig
    val cons : 'a -> 'b -> 'a * 'b
    val show : ('a -> string) -> ('b -> string) -> 'a * 'b -> string
    val fst : 'a * 'b -> 'a
    val snd : 'a * 'b -> 'b
    val map1 : ('a -> 'b) -> 'a * 'c -> 'b * 'c
    val map2 : ('a -> 'b) -> 'c * 'a -> 'c * 'b
    val map : ('a -> 'b) -> ('c -> 'd) -> 'a * 'c -> 'b * 'd
    val swap : 'a * 'b -> 'b * 'a
    val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c
    val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c
  end
val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c
val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c
module Option :
  sig
    val some : 'a -> 'a option
    val is_some : 'a option -> bool
    val is_none : 'a option -> bool
    val get_some : 'a option -> 'a
    val show : ('a -> string) -> 'a option -> string
    val map : ('a -> 'b) -> 'a option -> 'b option
    val map_default : ('a -> 'b) -> 'b -> 'a option -> 'b
    val bind : 'a option -> ('a -> 'b option) -> 'b option
    val filter : ('a -> bool) -> 'a option -> 'a option
    val default : 'a -> 'a option -> 'a
    val try_some : ('a -> 'b) -> 'a -> 'b option
    module Infix :
      sig
        val ( >>= ) : 'a option -> ('a -> 'b option) -> 'b option
        val ( |? ) : 'a option -> 'a -> 'a
      end
    val ( >>= ) : 'a option -> ('a -> 'b option) -> 'b option
    val ( |? ) : 'a option -> 'a -> 'a
  end
val ( >>= ) : 'a option -> ('a -> 'b option) -> 'b option
val ( |? ) : 'a option -> 'a -> 'a
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
    val show : ('a -> string) -> 'a list -> string
    val flat_map : ('a -> 'b list) -> 'a list -> 'b list
    val find_map : ('a -> 'b option) -> 'a list -> 'b option
    val choose : 'a list -> 'a option
  end
module Set :
  sig
    type 'a t
    val of_list : 'a list -> 'a t
    val to_list : 'a t -> 'a list
    val union : 'a t -> 'a t -> 'a t
    val inter : 'a t -> 'a t -> 'a t
    val diff : 'a t -> 'a t -> 'a t
    val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val map : ('a -> 'b) -> 'a t -> 'b t
    val cardinal : 'a t -> int
    val is_empty : 'a t -> bool
    val mem : 'a -> 'a t -> bool
    val for_all : ('a -> bool) -> 'a t -> bool
    val exists : ('a -> bool) -> 'a t -> bool
    val remove : 'a -> 'a t -> 'a t
    val choose : 'a t -> 'a option
    val add : 'a -> 'a t -> 'a t
    val empty : 'a t
    val find : ('a -> bool) -> 'a t -> 'a option
  end
module String :
  sig
    val concat : string -> string list -> string
    val escaped : 'a -> 'a
    val explode : string -> char list
    val implode : char list -> string
    val cons : char -> string -> string
    val of_char : char -> string
  end
module Map :
  sig
    type ('k, 'v) t = Empty | Node of 'k * 'v * ('k, 'v) t * ('k, 'v) t
    val empty : ('a, 'b) t
    val to_list : ('a, 'b) t -> ('a * 'b) list
    val show : ('a -> string) -> ('b -> string) -> ('a, 'b) t -> string
    val add : 'a -> 'b -> ('a, 'b) t -> ('a, 'b) t
    val of_list : ('a * 'b) list -> ('a, 'b) t
    val find : 'a -> ('a, 'b) t -> 'b option
    val mem : 'a -> ('a, 'b) t -> bool
    val min : ('a, 'b) t -> ('a * 'b) option
    val max : ('a, 'b) t -> ('a * 'b) option
    val remove : 'a -> ('a, 'b) t -> ('a, 'b) t
    val map : ('a -> 'b) -> ('c, 'a) t -> ('c, 'b) t
    val filter : ('a * 'b -> bool) -> ('a, 'b) t -> ('a, 'b) t
    val merge :
      ('a -> 'b option -> 'c option -> 'd option) ->
      ('a, 'b) t -> ('a, 'c) t -> ('a, 'd) t
  end
