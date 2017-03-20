open Printf
open String

let rec superfib n =
  if n <= 2 then n
  else (superfib (n - 1)) + (superfib (n - 2)) + (superfib (n - 3))

let inv_cantor n =
  (* Zunächst gehen wir nur entlang einer Achse *)
  let rec fast_upward x k =
    let dist = n - k in
    if dist > x then
      fast_upward (x + 1) (k + x + 1)
      (* Wenn wir die richtige Schiene gefunden haben, können
         wir das Ergebnis einfach berechnen *)
    else (x - dist, dist)
  in fast_upward 0 0

(* Langsame Version, die alle Cantor-Werte besucht *)
let inv_cantor_slow n =
  let rec inv_cantor y x k =
    if k == n then (x, y) else
    if x == 0 then inv_cantor 0 (y + 1) (k + 1) else
      inv_cantor (y + 1) (x - 1) (k + 1)
  in inv_cantor 0 0 0

let rec is_insert f x = function
    hd :: tl -> if f x hd then
      (* Haben wir die Einfügestelle gefunden, fügen wir ein... *)
      x :: hd :: tl else
      (* ... sonst suchen wir weiter.  *)
      hd :: (is_insert f x tl) 
  | [] -> [x]

let insertion_sort l f =
  (* Innere Funktion, die jeweils den bereits sortierten Teil
     der Liste erhält *)
  let rec insertion_sort sorted = function
      hd :: tl -> insertion_sort (is_insert f hd sorted) tl
    | [] -> sorted in
  insertion_sort [] l

let scale_apply = fun vs fs ->
  let rec scale_apply acc vs = function
      f :: ftl -> (match vs with
          a :: b :: c :: vstl -> a * (f b) + c :: scale_apply acc vstl ftl
        | [_; _] | [_] | [] -> acc)
    | [] -> acc
  in scale_apply [] vs fs

type person = {
  name : string;
  age : int;
}
[@@deriving show]

type tree =
    Node of tree * tree * person
  | Leaf
[@@deriving show]

let singleton p = Node(Leaf, Leaf, {name = p.name; age = p.age})

let rec insert p = function
  (* Je nach Ordnung der Namen der einzufügenden Person und der Person
     im aktuellen Knoten, steigen wir in die richtige Richtung ab. *)
    Node(lhs, rhs, p_node) -> let v = compare p.name p_node.name in
    if v == -1 then Node(insert p lhs, rhs, p_node) else
    if v == 0 then Node(lhs, rhs, p_node) else
      Node(lhs, insert p rhs, p_node)
  (* An einem Blatt schließlich wird eingefügt. *)
  | Leaf -> singleton p

let rec to_sorted_list = function
    Node(lhs, rhs, p_node) ->
    (to_sorted_list lhs) @ [p_node] @ (to_sorted_list rhs)
  | Leaf -> []


let print_list l =
  print_endline ([%derive.show: int list] l)

let () =
  printf "%d\n" (superfib 5);
  print_list (insertion_sort [1; 10; 5; -100; 99; 3; -1] (<=));
  print_list (insertion_sort [1; 10; 5; -100; 99; 3; -1] (>=));
  print_list (insertion_sort [1; 10; 5; -100; 99; 3; -1] (fun x y -> abs x <= abs y));
  let (x, y) = inv_cantor 11 in printf "(%d, %d)\n" x y;
  print_list (scale_apply [1; 10; 5; -100; 99; 3; -1] [(fun x -> 2*x + 1); (fun x -> 42)]);
  let t = insert {name = "Inge"; age = 171}
      (insert {name = "Hugo"; age = 27}
         (singleton {name = "Lisa"; age = 11})) in
  print_endline ([%derive.show: tree] t);
