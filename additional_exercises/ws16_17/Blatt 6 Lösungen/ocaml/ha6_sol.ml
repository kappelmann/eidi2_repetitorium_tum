open Printf
open Batteries.List
open Ha6_angabe
open Batteries.Option

(* Eine Linse kann dazu verwendet werden, das Feld eines Records wie einen
   Wert herumzureichen. Sie bietet dazu eine Funktion zum Lesen und eine Funktion
   zum Schreiben des jeweiligen Feldes an. *)
type subtree_lens = {
  get : avl_tree_node -> avl_tree;
  set : avl_tree_node -> avl_tree -> avl_tree_node
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

let valid_avl t =
  (* Liefert das äußerste Element eines Baumes; je nach Lise also das größte
     bzw. kleinste Element *)
  let rec outer lens = function
      Node node ->
      let next = lens.get node in
      (match next with
         Node next_node -> outer lens next
       | Leaf -> Some node.key)
    | Leaf -> None in
  let max t = outer lens_right t in
  let min t = outer lens_left t in
  let rec height = function
      Node n -> (Pervasives.max (height n.left) (height n.right)) + 1
    | Leaf -> 0 in
  let rec valid_inner = function
      Node n ->
      (* Bei einem validen Baum sind die Teilbäume valide *)
      let valid_left = valid_inner n.left in
      let valid_right = valid_inner n.right in
      (* Bei einem validen Baum ist der Schlüssel der Wurzel mindestens so groß
         wie das größte Element im linken Teilbaum und höchstens so groß wie das größte
         Element im rechten Teilbaum. *)
      let bigger_than_left = map_default (fun key -> key <= n.key) true (max n.left) in
      let smaller_than_right = map_default (fun key -> n.key <= key) true (min n.right) in
      let depth_left = height n.left in
      let depth_right = height n.right in
      (* Bei einem validen Baum entspricht die Balancierung dem Höhenunterschied und
         hat einen Wert zwischen -1 und 1. *)
      let balance = depth_right - depth_left in
      valid_left && valid_right && bigger_than_left && smaller_than_right
      && balance < 2 && balance > (-2) && balance == n.balance
    | Leaf -> true
  in valid_inner t

let dot t =
  (* Jeder Knoten erhält eine ID. Die nächste freie ID wird in 'acc' übergeben.
     dot_inner gibt den Graph im Dot-Format, die nächste freie ID und die ID des
     Wurzelknotens zurück. *)
  let rec dot_inner dot acc = function
      Node {key; balance; left; right} ->
      let (dot, acc, id_left) = dot_inner dot acc left in
      let (dot, acc, id_right) = dot_inner dot acc right in
      let id = acc in
      let dot = (sprintf "  %d [label=\"%d\"];\n  %d -> %d;\n  %d -> %d;\n"
                   id key id id_left id id_right)::dot in
      (dot, acc + 1, id)
    | Leaf ->
      let id = acc in
      let dot = (sprintf "  %d [label=Leaf];\n" id)::dot in
      (dot, acc + 1, id)
  in let (inner, _, _) = dot_inner [] 0 t in
  sprintf "digraph {\n%s}" (String.concat "" inner)

(* Links- und Rechtsrotationen unterscheiden sich genau in der Wahl der Felder;
   durch die Wahl der entsprechenden Linsen lässt sich daher die Rotationsrichtung
   konfigurieren. *)
let lenses = function
    Left -> (lens_left, lens_right)
  | Right -> (lens_right, lens_left)

let rotate_single dir t =
  let (first, second) = lenses dir in
  match t with
    Node a -> let b = match (second.get a) with
        Node b -> b
      | Leaf -> failwith "Cannot rotate leaf" in
    (* Der Baum wird entsprechend der Regeln für einfache Rotationen umorganisiert. *)
    let a' = {(second.set a (first.get b)) with balance = 0} in
    let b' = {(first.set b (Node a')) with balance = 0} in
    Node b'
  | Leaf -> failwith "Cannot rotate leaf"

let rotate_double dir t =
  let (first, second) = lenses dir in
  match t with
    Node a -> (match (second.get a) with
        Node b -> (
          match (first.get b) with
            Node x ->
            let sgn = signum a.balance in
            (* Der Baum wird entsprechend der Regeln für doppelte Rotationen
               umorganisiert. *)
            let a' = {(second.set a (first.get x))
                      with balance = if (x.balance == -sgn || x.balance == 0) then 0
                             else -sgn} in
            let b' = {(first.set b (second.get x))
                      with balance = if (x.balance == -sgn && x.balance != 0) then sgn
                             else 0} in
            let x' = first.set x (Node a') in
            let x' = second.set x' (Node b') in
            Node {x' with balance = 0}
          | Leaf -> failwith "Cannot rotate leaf"
        )
      | Leaf -> failwith "Cannot rotate leaf"
    )
  | Leaf -> failwith "Cannot rotate leaf"

let rebalance t = match t with
    Node node -> (
      let dir = match node.balance with
        -2 -> Some Right
        | 2 -> Some Left
        | _ -> None
      in match dir with
        Some dir -> (
          let (first, second) = lenses dir in
          match second.get node with
          (* Einfache Rotationen werden genau dann ausführt, wenn das Vorzeichen
             der Balancierung von Mutter und Kind gleich sind *)
            Node b -> if (signum node.balance) == (signum b.balance)
            then rotate_single dir t
            else rotate_double dir t
          | Leaf -> failwith "Cannot rotate leaf"
        )
      | None -> t
    )
  | Leaf -> t

let insert x t =
  let balance_of = function
      Node {balance} -> balance
    | Leaf -> 0 in
  let rec insert_inner x = function
      Node node ->
      (* Je nach Verhältnis von 'x' zu 'node.key' muss nach links oder rechts
         abgestiegen werden. Zusätzliches Gewicht auf der jeweiligen Seite wird
         links von der Balancierung abgezogen, zählt recht jedoch zu ihr. *)
      let (lens, adapt_balance) = if x <= node.key then
          (lens_left, (-)) else
          (lens_right, (+)) in
      let (extra_weight, _new) = insert_inner x (lens.get node) in
      assert (extra_weight >= 0 && extra_weight <= 1);
      let balance_adapted = adapt_balance node.balance extra_weight in
      let tree_new = Node {(lens.set node _new) with balance = balance_adapted} in
      let tree_new_rotated = rebalance tree_new in
      let balance_new = balance_of tree_new_rotated in
      (* Zusätzliches Gewicht entsteht nur auf der Blatt-Ebene; ist 'extra_weight' also
         0, müssen wir diesen Wert propagieren. Ist 'extra_weight' 1, so entspricht der Absolutwert
         der Balancierung dem zusätzlichen Gewicht, welches der aktuelle Teilbaum aufweist. *)
      (extra_weight * abs balance_new, tree_new_rotated)
    | Leaf -> (1, singleton x)
  in let (_, t) = insert_inner x t in t
