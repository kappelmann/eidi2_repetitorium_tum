open Ha11_angabe

module MakeHashMap (H : Hashable) : (Map with type key = H.key) = struct
  type key = H.key
  type 'v t = (key * 'v) list array

  let create () = Array.make 37 []

  let size hm =
    Array.fold_left (fun acc bucket -> acc + List.length bucket) 0 hm

  let filtered_bucket hm k =
    let hash = (H.hash k) mod (Array.length hm) in
    let bucket = hm.(hash) in
    let filtered = List.filter (fun (k', _) -> k != k') bucket in
    (hash, filtered)

  let insert hm k v =
    let (hash, filtered) = filtered_bucket hm k in
    hm.(hash) <- (k, v)::filtered;
    hm

  let remove hm k =
    let (hash, filtered) = filtered_bucket hm k in
    hm.(hash) <- filtered;
    hm

  let lookup hm k =
    let hash = (H.hash k) mod (Array.length hm) in
    let bucket = hm.(hash) in
    try
      let (_, v) = List.find (fun (k', v) -> k = k') bucket in
      Some v
    with Not_found -> None
end

module IntHashMap = MakeHashMap (struct
    type key = int
    let hash = Hashtbl.hash
  end)

module MakeTreeMap (C : Comparable) : (Map with type key = C.key) = struct
  type key = C.key
  type 'v t = (key * 'v) bin_tree

  let create () = Leaf

  let size hm =
    let rec inner = function
        Node n -> 1 + (inner n.left) + (inner n.right)
      | Leaf -> 0
    in inner hm

  let compare_tuple hm (k1, _) (k2, _) = C.compare k1 k2

  let insert hm k v = insert hm (k, v) (compare_tuple hm)

  let remove hm k = remove hm k (fun k (k', _) -> C.compare k k')

  let rec lookup hm k = match hm with
      Node { data = (k', v); left; right} ->
      let comp = C.compare k k' in
      if comp = 0 then Some v
      else if comp < 0 then lookup left k
      else lookup right k
    | Leaf -> None
end

module IntTreeMap = MakeTreeMap (struct
    type key = int
    let compare = Pervasives.compare
  end)

let () =
  let opt_str = (function
        Some x -> x
      | None -> "None") in
  let m = IntHashMap.create () in
  let m = IntHashMap.insert m 5 "Hallo" in
  let m = IntHashMap.insert m 7 "Hugo" in
  Printf.printf "%s\n" (opt_str (IntHashMap.lookup m 5));
  Printf.printf "%s\n" (opt_str (IntHashMap.lookup m 20));
  Printf.printf "%s\n" (opt_str (IntHashMap.lookup m 7));
  let m = IntTreeMap.create () in
  let m = IntTreeMap.insert m 5 "Hallo" in
  let m = IntTreeMap.insert m 7 "Hugo" in
  Printf.printf "%s\n" (opt_str (IntTreeMap.lookup m 5));
  Printf.printf "%s\n" (opt_str (IntTreeMap.lookup m 20));
  Printf.printf "%s\n" (opt_str (IntTreeMap.lookup m 7))

let (%) g f x = g (f x)
let const x _ = x
let id x = x

module Lift (B : Base) : Extended with type 'a t = 'a B.t = struct
  include B
  (* 'iter' muss die Elemente von links besuchen, während fold hier
     von rechts faltet. Aus diesem Grund wird hier ein Funktionsobjekt
     aufgebaut, welches ganz am Ende aufgerufen wird. *)
  let iter f x = fold (fun x acc -> (fun () -> f x; acc())) x (fun () -> ()) ()
  let map f x = fold (insert%f) x empty
  let filter f x = fold (fun a b -> if f a then insert a b else b) x empty
  let append x y = fold insert x y
  let flatten x = fold append x empty
  let to_list x = fold List.cons x []
  let of_list x = List.fold_right insert x empty
end

module List = struct
  type 'a t = 'a list
  let empty = []
  let insert = List.cons
  let fold = List.fold_right
end

module ExtendedList = Lift (List)

module SearchTree = struct
  type 'a t = Leaf | Node of ('a * 'a t * 'a t)
  let empty = Leaf
  let rec insert x = function
    | Leaf -> Node (x, Leaf, Leaf)
    | Node (y, a, b) ->
      if x < y then Node (y, insert x a, b) else Node (y, a, insert x b)
  let rec fold f = function (* pre-order traversal: node, left, right *)
    | Leaf -> id
    | Node (v, l, r) -> fun a -> fold f r (fold f l (f v a))
end

module ExtendedSearchTree = Lift (SearchTree)

let () =
  let l = 4::2::1::[] in
  ExtendedList.iter (fun x -> Printf.printf "%d\n" x) l;
  let t = ExtendedSearchTree.insert 4
      (ExtendedSearchTree.insert 6
         (ExtendedSearchTree.insert 1
            ExtendedSearchTree.empty)) in
  ExtendedSearchTree.iter (fun x -> Printf.printf "%d\n" x) t