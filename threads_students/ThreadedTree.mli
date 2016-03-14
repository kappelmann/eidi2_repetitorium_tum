let todo _ = failwith "TODO, implement this awesome thing."

(* This tree only stores values in its leaf nodes! Inner nodes just store references to
 * a left and right child. The leaf nodes are not ordered, that means the min. and max. value
 * of the tree must not be at the left-most or right-most leaf.
 * There are no empty leafs and there are also no empty trees.*)
type 'a tree = todo

(* Searches for the smallest value of the tree. For every inner node two new
 * threads should be created to search for the min. value in the left and right subtree. *)
val min : todo
