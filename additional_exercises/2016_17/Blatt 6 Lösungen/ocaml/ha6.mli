val valid_avl : Ha6_angabe.avl_tree -> bool
val dot : Ha6_angabe.avl_tree -> string
val rotate_single :
  Ha6_angabe.direction -> Ha6_angabe.avl_tree -> Ha6_angabe.avl_tree
val rotate_double :
  Ha6_angabe.direction -> Ha6_angabe.avl_tree -> Ha6_angabe.avl_tree
val rebalance : Ha6_angabe.avl_tree -> Ha6_angabe.avl_tree
val insert : int -> Ha6_angabe.avl_tree -> Ha6_angabe.avl_tree
