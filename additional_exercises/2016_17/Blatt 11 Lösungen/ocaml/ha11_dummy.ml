open Ha11_angabe

module MakeHashMap (H : Hashable) : (Map with type key = H.key) = struct
  type key = H.key
  type 'v t = unit

  let create () = failwith "Todo"

  let size hm = failwith "Todo"

  let insert hm k v = failwith "Todo"

  let remove hm k = failwith "Todo"

  let lookup hm k = failwith "Todo"
end

module IntHashMap = MakeHashMap (struct
    type key = int
    let hash k = failwith "Todo" 
  end)

module MakeTreeMap (C : Comparable) : (Map with type key = C.key) = struct
  type key = C.key
  type 'v t = unit

  let create () = failwith "Todo"

  let size hm = failwith "Todo"

  let insert hm k v = failwith "Todo"

  let remove hm k = failwith "Todo"

  let rec lookup hm k = failwith "Todo"
end

module IntTreeMap = MakeTreeMap (struct
    type key = int
    let compare x y = failwith "Todo"
  end)

module Lift (B : Base) : Extended with type 'a t = 'a B.t = struct
  include B
  let iter f x = failwith "Todo"
  let map f x = failwith "Todo"
  let filter f x = failwith "Todo"
  let append x y = failwith "Todo"
  let flatten x = failwith "Todo"
  let to_list x = failwith "Todo"
  let of_list x = failwith "Todo"
end

module List = struct
  type 'a t = unit
  let empty = ()
  let insert a b = failwith "Todo"
  let fold a b c = failwith "Todo"
end

module ExtendedList = Lift (List)

module SearchTree = struct
  type 'a t = unit
  let empty = ()
  let rec insert a  = failwith "Todo"
  let rec fold a b c = failwith "Todo"
end

module ExtendedSearchTree = Lift (SearchTree)