val superfib : int -> int
val inv_cantor : int -> int * int
val is_insert : (int -> int -> bool) -> int -> int list -> int list
val insertion_sort : int list -> (int -> int -> bool) -> int list
val scale_apply : int list -> (int -> int) list -> int list
type person = { name : string; age : int; }
type tree = Node of tree * tree * person | Leaf
val singleton : person -> tree
val insert : person -> tree -> tree
val to_sorted_list : tree -> person list
