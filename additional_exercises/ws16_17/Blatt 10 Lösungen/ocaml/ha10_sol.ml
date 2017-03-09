open Ha10_angabe

module HashMap : Map = struct
  type ('k, 'v) t = {
    table : ('k * 'v) list array;
    hash : 'k -> int;
  }

  let create ki = {
    table = Array.make 17 [];
    hash = ki.Ha10_angabe.hash;
  }

  let size hm =
    Array.fold_left (fun acc bucket -> acc + List.length bucket) 0 hm.table

  let insert hm k v =
    let hash = (hm.hash k) mod (Array.length hm.table) in
    let bucket = hm.table.(hash) in
    let filtered = List.filter (fun (k', _) -> k != k') bucket in
    hm.table.(hash) <- (k, v)::filtered;
    hm

  let remove hm k =
    let hash = (hm.hash k) mod (Array.length hm.table) in
    let bucket = hm.table.(hash) in
    let filtered = List.filter (fun (k', _) -> k != k') bucket in
    hm.table.(hash) <- filtered;
    hm
end

module TreeMap : Map = struct
  type ('k, 'v) t = {
    tree : ('k * 'v) bin_tree;
    compare : 'k -> 'k -> int;
  }

  let create ki = {
    tree = Leaf;
    compare = ki.Ha10_angabe.compare
  }

  let size hm =
    let rec inner = function
        Node n -> 1 + (inner n.left) + (inner n.right)
      | Leaf -> 0
    in inner hm.tree

  let compare_tuple hm (k1, _) (k2, _) =
    hm.compare k1 k2

  let insert (hm : ('k, 'v) t) k v =
    let inserted = insert hm.tree (k, v) (compare_tuple hm) in
    {hm with tree = inserted}

  let remove hm k =
    let removed = remove hm.tree k (fun k (k', _) -> hm.compare k k') in
    {hm with tree = removed}
end

module SparseVector : Vector = struct
  type t = (int*int) list
  [@@deriving show]

  let (%) g f x = g (f x)
  let const x _ = x

  let empty = []

  (* Ähnlich zu List.fold_left2, behandelt aber fehlende Einträge
     wie Mappings auf 0  *)
  let rec fold_left2 f acc a b =
    match (a, b) with
      (a_i, a_v)::a_tl, (b_i, b_v)::b_tl ->
      if a_i = b_i then
        let acc = f acc a_i a_v b_v in fold_left2 f acc a_tl b_tl
      else if a_i < b_i then
        let acc = f acc a_i a_v 0 in fold_left2 f acc a_tl b
      else
        let acc = f acc b_i 0 b_v in fold_left2 f acc a b_tl
    | (a_i, a_v)::a_tl, [] ->
      let acc = f acc a_i a_v 0 in fold_left2 f acc a_tl []
    | [], (b_i, b_v)::b_tl ->
      let acc = f acc b_i 0 b_v in fold_left2 f acc b_tl []
    | _ -> acc

  (* Kombination aus List.map2 und List.mapi; außerdem werden
     Nullwerte ignoriert. *)
  let rec map2i f a b = 
    let prep i v xs =
      if v = 0 then xs (* Nullwerte ignorieren! *)
      else (i, v)::xs in
    List.rev (fold_left2 (fun acc i a b -> prep i (f i a b) acc) [] a b)

  let set i v vec =
    map2i (fun i' a b -> if i = i' then b else a) vec [i, v]

  let binop op = map2i (fun _ a b -> op a b)

  let add = binop (+)

  let mul r t = if r = 0 then []
    else List.map (fun (i, x) -> (i, x*r)) t

  let sprod a b = List.fold_right ((+) % snd) (binop ( * ) a b) 0
end

let () =
  let a = SparseVector.set 0 8 SparseVector.empty in
  let b = SparseVector.set 1 5 SparseVector.empty in
  let c = SparseVector.set 1 8 SparseVector.empty in
  let d = SparseVector.set 1 2 SparseVector.empty in
  let r = SparseVector.sprod
      (SparseVector.mul 4 (SparseVector.add a b))
      (SparseVector.mul 3 (SparseVector.add c d))
  in
  Printf.printf "%d\n" r