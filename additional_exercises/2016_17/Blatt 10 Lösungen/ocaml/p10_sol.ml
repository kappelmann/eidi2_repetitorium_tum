module type Heap = sig
  type 'a t
  exception Empty

  val create : unit -> 'a t
  val is_empty : 'a t -> bool
  val insert : 'a -> 'a t -> 'a t
  val delete_max : 'a t -> ('a * 'a t)
end


module BinTreeHeap : Heap = struct
  type 'a btree = Leaf | Node of int * 'a * 'a btree * 'a btree 
  type 'a t = 'a btree
  exception Empty

  let create () = Leaf

  let is_empty = function
      Leaf -> true
    | _ -> false

  let min_way = function
      Leaf -> 0
    | Node (c, _, _, _) -> c

  let rec insert x = function
      Leaf -> Node (1, x, Leaf, Leaf)
    | Node(_,x',t1,t2) -> 
      let (s,ns) = if x > x' then (x', x) else (x, x') in
      let (t1,t2) = if min_way t1 <= min_way t2 then
          (insert s t1, t2) 
        else (t1, insert s t2) in
      Node(1 + min (min_way t1) (min_way t2), ns, t1, t2)


  let rec repair t1 t2 =
    match (t1, t2) with
      (Leaf, t) | (t, Leaf) -> t
    | (Node (_, x1, t1', t1''), Node (_, x2, t2', t2'')) -> 
      let (x, t1, t2) = if x1 > x2 then 
          (x1, repair t1' t1'', t2) 
        else (x2, t1, repair t2' t2'') in
      Node(1 + min (min_way t1) (min_way t2), x, t1, t2)

  let delete_max = function
      Leaf -> raise Empty
    | Node(_, x, t1, t2) -> (x, repair t1 t2)
end

let heapsort l = 
  let rec build_list acc heap =
    if BinTreeHeap.is_empty heap then
      acc else
      let (x,heap) = BinTreeHeap.delete_max heap in
      build_list (x::acc) heap in
  let build_list l = build_list [] l in
  build_list (List.fold_left (fun heap x ->
      BinTreeHeap.insert x heap) (BinTreeHeap.create ()) l) 

let factors n =
  let rec aux d n =
    if n = 1 then [] else
    if n mod d = 0 then
      d :: aux d (n / d) else
      aux (d+1) n
  in
  aux 2 n;;