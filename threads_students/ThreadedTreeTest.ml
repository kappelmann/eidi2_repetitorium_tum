open ThreadedTree

let t = Leaf 100
let m = min t
let e = 100
let _ = if e<>m then failwith "Your tree delivered the wrong result for a single leaf tree."
let t = Node( Node( Node( Leaf (-10), Leaf 10), Node ( Leaf 3, Leaf (-11))), Leaf 0)
let m = min t
let e = (-11)
let _ = if e=m then print_string "Your threaded tree works like a charm! +10p" else failwith "Your tree delivered the wrong result for a single leaf tree."
