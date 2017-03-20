type 'k key_info = {
  compare : 'k -> 'k -> int;
  hash : 'k -> int
}

module type Map = sig
  type ('k, 'v) t

  val create : 'k key_info -> ('k, 'v) t
  val size : ('k, 'v) t -> int
  val insert : ('k, 'v) t -> 'k -> 'v -> ('k, 'v) t
  val remove : ('k, 'v) t -> 'k -> ('k, 'v) t
end

type 'a bin_tree =
    Node of { 
      data : 'a;
      left : 'a bin_tree;
      right : 'a bin_tree;
    }
  | Leaf
[@@deriving show]

let rec insert t x compare = match t with
    Node n ->
    let comp = compare x n.data in
    if comp = 0 then Node {n with data = x}
    else if comp < 0 then Node {n with left = insert n.left x compare}
    else Node {n with right = insert n.right x compare}
  | Leaf -> Node { data = x; left = Leaf; right = Leaf } 

(* Entfernt ein Element x aus dem Baum t, sofern vorhanden *)
let remove t x compare =
  let rec remove_max = function
      Node { data; left; right = Node right } -> let (key', next) = remove_max (Node right) in
      (key', Node {data; left; right = next})
    | Node { data; left; right = Leaf } -> (data, left)
    | Leaf -> failwith "Cannot remove value from leaf"
  in let rec remove_inner = function
        Node n when compare x n.data < 0 -> Node {n with left = remove_inner n.left}
      | Node n when compare x n.data > 0 -> Node {n with right = remove_inner n.right}
      | Node {left; right = Leaf} -> left
      | Node {left = Leaf; right} -> right
      | Node {left; right} -> (let (data, next) = remove_max left in
                               Node {data = data; left = next; right})
      | Leaf -> Leaf
  in remove_inner t

module type Vector = sig
  type t

  val empty : t
  val set : int -> int -> t -> t
  val add : t -> t -> t
  val mul : int -> t -> t
  val sprod : t -> t -> int
end 