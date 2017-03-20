type avl_tree =
    Node of avl_tree_node
  | Leaf
and avl_tree_node = { 
  key : int;
  balance : int;
  left : avl_tree;
  right : avl_tree;
}
[@@deriving show]

let singleton key = Node {
    key = key;
    balance = 0;
    left = Leaf;
    right = Leaf;
  }

let signum x =
  if x > 0 then 1
  else if x < 0 then -1
  else 0

type direction = Left | Right
[@@deriving show]