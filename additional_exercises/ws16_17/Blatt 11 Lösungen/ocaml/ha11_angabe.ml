module type Hashable = sig
  type key

  val hash : key -> int
end

module type Comparable = sig
  type key

  val compare : key -> key -> int
end

module type Map = sig
  type key
  type 'v t

  val create : unit -> 'v t
  val size : 'v t -> int
  val insert : 'v t -> key -> 'v -> 'v t
  val remove : 'v t -> key -> 'v t
  val lookup : 'v t -> key -> 'v option
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

module type Base = sig
  type 'a t
  val empty : 'a t (* nullary/nonrec. constr. *)
  val insert : 'a -> 'a t -> 'a t (* rec. constr. *)
  val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
end

module type Extended = sig
  include Base
  val iter : ('a -> unit) -> 'a t -> unit
  val map : ('a -> 'b) -> 'a t -> 'b t
  val filter : ('a -> bool) -> 'a t -> 'a t
  val append : 'a t -> 'a t -> 'a t
  val flatten : 'a t t -> 'a t
  val to_list : 'a t -> 'a list
  val of_list : 'a list -> 'a t
end