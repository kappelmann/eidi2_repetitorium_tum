open Tree
let t = Tree (1,[])
let r = size t
let e = 1
let _ = if e<>r then failwith "Your tree delivered a wrong size for an almost empty tree"

let t = Tree ( 4, [ (Tree (2,[])); (Tree((-3),[ (Tree ((1,[])))])) ] )
let r = size t
let e = 4
let _ = if e<>r then failwith "Your tree delivered a wrong size"

let r = map (fun x -> x+1) t
let e = Tree ( 5, [ (Tree (3,[])); (Tree((-2),[ (Tree ((2,[])))])) ] )
let _ = if e<>r then failwith "Your map function is not working correctly"

let r = to_list t
let rec b = function [] -> print_string "\n"
        | x::xs -> print_int x; print_string "; "; b xs
let _ = b r
