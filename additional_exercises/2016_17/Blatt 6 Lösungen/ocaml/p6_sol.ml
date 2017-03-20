type bin_tree =
    Node of bin_tree_node
  | Leaf
and bin_tree_node = { 
  key : int;
  left : bin_tree;
  right : bin_tree;
}
[@@deriving show]

let rec insert key = function
    Node n ->
    if key <= n.key then Node {n with left = insert key n.left}
    else Node {n with right = insert key n.right}
  | Leaf -> Node { key; left = Leaf; right = Leaf } 

let rec sum = function
    Node {key; left; right} -> key + (sum left) + (sum right)
  | Leaf -> 0

(* Eine Linse kann dazu verwendet werden, das Feld eines Records wie einen
   Wert herumzureichen. Sie bietet dazu eine Funktion zum Lesen und eine Funktion
   zum Schreiben des jeweiligen Feldes an. *)
type subtree_lens = {
  get : bin_tree_node -> bin_tree;
  set : bin_tree_node -> bin_tree -> bin_tree_node
}

(* Definiert eine Liste für den rechten Teilbaum *)
let lens_right = {
  get = (fun node -> node.right);
  set = fun node t -> {node with right = t}
}

(* Definiert eine Liste für den linken Teilbaum *)
let lens_left = {
  get = (fun node -> node.left);
  set = fun node t -> {node with left = t}
}

(* Liefert das äußerste Element eines Baumes; je nach Lise also das größte
   bzw. kleinste Element *)
let rec outer lens = function
    Node node ->
    let next = lens.get node in
    (match next with
       Node next_node -> outer lens next
     | Leaf -> Some node.key)
  | Leaf -> None

let max t = outer lens_right t
let min t = outer lens_left t

(* Entfernt ein Element x aus dem Baum t, sofern vorhanden *)
let remove x t =
  (* Entfernt das größte (rechteste) Element im Teilbaum; diese Funtion liefert
     den Schlüssel des entfernten Elementes sowie den neuen Baum zurück. *)
  let rec remove_max = function
      Node { key; left; right = Node right } -> let (key', next) = remove_max (Node right) in
      (key', Node {key; left; right = next})
    | Node { key; left; right = Leaf } -> (key, left)
    | Leaf -> failwith "Cannot remove value from leaf"
  in let rec remove_inner = function
        Node n when x < n.key -> Node {n with left = remove_inner n.left}
      | Node n when x > n.key -> Node {n with right = remove_inner n.right}
      (* Wenn wir das Element, welches gelöscht werden soll, gefunden haben,
         müssen wir drei Fälle unterscheiden:
         1. Das Element hat nur Blätter als Kinder: In diesem Fall kann es einfach
            entfernt werden, wir geben ein Blatt zurück.
         2. Einer der beiden Unterbäume ist ein Blatt, einer nicht: Wir ersetzen den
            aktuellen Knoten durch denjenigen Nachfolger, der kein Blatt ist.Arg
         3. Beide Unterbäume sind keine Blätter: Wir entfernen das größte Element im
            linekn Teilbaum und ersetzen den Schlüssel des aktuellen Knotens durch den
            so entfernten größten Schlüssel im linken Teilbaum. *)
      | Node {left; right = Leaf} -> left
      | Node {left = Leaf; right} -> right
      | Node {left; right} -> (let (key, next) = remove_max left in
                               Node {key = key; left = next; right})
      | Leaf -> Leaf
  in remove_inner t

let rec height = function
    Node n -> (Pervasives.max (height n.left) (height n.right)) + 1
  | Leaf -> 0


let () =
  let t = insert (-1)
      (insert 5 (
          insert 3 (
            insert 10 (
              insert 2 (
                insert 9
                  (insert (-5) (
                      insert 100 Leaf))))))) in
  let t' = remove 9 t in
  let t'' = remove 3 t' in
  let t''' = remove (-5) t'' in
  print_endline ([%derive.show: bin_tree] t);
  print_endline ([%derive.show: bin_tree] t');
  print_endline ([%derive.show: bin_tree] t'');
  print_endline ([%derive.show: bin_tree] t''')