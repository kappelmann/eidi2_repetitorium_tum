open Stack

module ListStack : Stack = struct
       type 'a stack = 'a list
       let empty = []
       let is_empty s = s=[]
       let push s e = e::s
       let pop = function [] -> (None,[]) | x::xs -> (Some x, xs)
end

let s = ListStack.empty
let s = ListStack.push s 1 
let s = ListStack.push s 2
let s = ListStack.push s 3

let (Some x, s) = ListStack.pop s
let _ = print_int x (* 3 printen *)
let (Some x, s) = ListStack.pop s
let _ = print_int x (* 2 printen *)
