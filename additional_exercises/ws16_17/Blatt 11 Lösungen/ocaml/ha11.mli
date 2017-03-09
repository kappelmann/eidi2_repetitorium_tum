module MakeHashMap :
  functor (H : Ha11_angabe.Hashable) ->
    sig
      type key = H.key
      type 'v t
      val create : unit -> 'v t
      val size : 'v t -> int
      val insert : 'v t -> key -> 'v -> 'v t
      val remove : 'v t -> key -> 'v t
      val lookup : 'v t -> key -> 'v option
    end
module IntHashMap :
  sig
    type key = int
    type 'v t
    val create : unit -> 'v t
    val size : 'v t -> int
    val insert : 'v t -> key -> 'v -> 'v t
    val remove : 'v t -> key -> 'v t
    val lookup : 'v t -> key -> 'v option
  end
module MakeTreeMap :
  functor (C : Ha11_angabe.Comparable) ->
    sig
      type key = C.key
      type 'v t
      val create : unit -> 'v t
      val size : 'v t -> int
      val insert : 'v t -> key -> 'v -> 'v t
      val remove : 'v t -> key -> 'v t
      val lookup : 'v t -> key -> 'v option
    end
module IntTreeMap :
  sig
    type key = int
    type 'v t
    val create : unit -> 'v t
    val size : 'v t -> int
    val insert : 'v t -> key -> 'v -> 'v t
    val remove : 'v t -> key -> 'v t
    val lookup : 'v t -> key -> 'v option
  end
module Lift :
  functor (B : Ha11_angabe.Base) ->
    sig
      type 'a t
      val empty : 'a t
      val insert : 'a -> 'a t -> 'a t
      val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
      val iter : ('a -> unit) -> 'a t -> unit
      val map : ('a -> 'b) -> 'a t -> 'b t
      val filter : ('a -> bool) -> 'a t -> 'a t
      val append : 'a t -> 'a t -> 'a t
      val flatten : 'a t t -> 'a t
      val to_list : 'a t -> 'a list
      val of_list : 'a list -> 'a t
    end
module List :
  sig
    type 'a t
    val empty : 'a t
    val insert : 'a -> 'a t -> 'a t
    val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  end
module ExtendedList :
  sig
    type 'a t
    val empty : 'a t
    val insert : 'a -> 'a t -> 'a t
    val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val iter : ('a -> unit) -> 'a t -> unit
    val map : ('a -> 'b) -> 'a t -> 'b t
    val filter : ('a -> bool) -> 'a t -> 'a t
    val append : 'a t -> 'a t -> 'a t
    val flatten : 'a t t -> 'a t
    val to_list : 'a t -> 'a list
    val of_list : 'a list -> 'a t
  end
module SearchTree :
  sig
    type 'a t
    val empty : 'a t
    val insert : 'a -> 'a t -> 'a t
    val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  end
module ExtendedSearchTree :
  sig
    type 'a t
    val empty : 'a t
    val insert : 'a -> 'a t -> 'a t
    val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val iter : ('a -> unit) -> 'a t -> unit
    val map : ('a -> 'b) -> 'a t -> 'b t
    val filter : ('a -> bool) -> 'a t -> 'a t
    val append : 'a t -> 'a t -> 'a t
    val flatten : 'a t t -> 'a t
    val to_list : 'a t -> 'a list
    val of_list : 'a list -> 'a t
  end
