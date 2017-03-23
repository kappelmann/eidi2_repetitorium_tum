open Stack

module Stack_Impl : Stack = struct
       type 'a stack = 'a list
       let empty = []
       let is_empty s = s=[]
       let push s e = e::s
       let pop = function [] -> (None,[]) | x::xs -> (Some x, xs)
end

let s = Stack_Impl.empty
let s = push s 1 
let s = push s 2
let s = push s 3

let (Some x, s) = pop s
print_int x (* 3 printen *)
let (Some x, s) = pop s
print_int x (* 2 printen *)
