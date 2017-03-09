
let rec superfib n = failwith "Todo"

let inv_cantor n = failwith "Todo"

let inv_cantor_slow n = failwith "Todo"

let rec is_insert f x l = failwith "Todo"

let insertion_sort l f = failwith "Todo"

let scale_apply vs fs = failwith "Todo"

type person = {
  name : string;
  age : int;
}
[@@deriving show]

type tree =
    Node of tree * tree * person
  | Leaf
[@@deriving show]

let singleton p = failwith "Todo"

let rec insert p t = failwith "Todo"

let rec to_sorted_list t = failwith "Todo" 
