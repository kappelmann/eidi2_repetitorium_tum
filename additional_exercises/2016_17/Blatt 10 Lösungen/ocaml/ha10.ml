open Ha10_angabe

module HashMap : Map = struct
  type ('k, 'v) t = unit

  let create ki = failwith "Todo"

  let size hm = failwith "Todo"

  let insert hm k v = failwith "Todo"

  let remove hm k = failwith "Todo"
end

module TreeMap : Map = struct
  type ('k, 'v) t = unit

  let create ki = failwith "Todo"

  let size hm = failwith "Todo"

  let insert hm k v = failwith "Todo"

  let remove hm k = failwith "Todo"
end

module SparseVector : Vector = struct
  type t = unit

  let empty = ()

  let set i v = failwith "Todo"

  let add a b = failwith "Todo"

  let mul r t = failwith "Todo"

  let sprod a b = failwith "Todo"
end