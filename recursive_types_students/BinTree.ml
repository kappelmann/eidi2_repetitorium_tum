open Tree
let todo _ = failwith "Ups, there is still something not implemented."

(*A binary tree that also stores information in its inner nodes.
 * The key and the value are equal.*)
module BinTree : Tree = struct
        type 'a t = todo
        let empty = todo 
        let to_list t = todo
	(*Duplicate values should not be inserted!*)
        let add p l = todo
        let rec fold f a t = todo
        let merge a b = todo
        let min t = todo
        let greater_than a n = todo
end
