open List

(*'a -> ('a -> -a) list -> 'a*)
fold_left (fun a b -> b a)

(*('a -> 'a) list -> 'a -> 'a*)
fold_right (fun b a -> b a)

(*'a -> 'a list -> 'a*)
fold_left (fun a b -> b)

(*'a -> 'b list -> 'a *)
fold_left (fun a b -> a)

(*'a option list -> 'a option list*)
filter (fun a -> a<>None)

(*(int -> bool) list -> (int -> bool) list*)
filter (fun a -> a 7)

(*('a -> 'b) * 'a -> 'b*)
let f x = match x with (x,y) -> x y

(*string list list -> string list*)
fold_left (fun a b -> a @ (filter (fun b -> b="") b)) []

(*unit -> Thread.t*)
let f x = Thread.create (fun x -> new_channel x) x

(*('a -> 'b list) -> 'a -> 'b list*)
let a f x = f x |> reverse

(*('a -> 'a) -> 'a -> 'a*)
let a f x = f(f x)

(*int list -> int*)
let a = fold_left (fun a x -> x) (0)

(*unit list*)
map (fun _ -> ()) [1;2;3]

(*string list*)
let x = "unwichtig" in map (fun _ -> (x)) [1;2;3]

(*int list*)
let x = "wichtig" in map (fun x -> (x)) [1;2;3]

(*('a * 'b list list) list -> 'b list -> 'b list*)
fold_right (fun (x,y) a -> fold_left (@) a y)

(*(('a list -> 'a list) * 'a list list) list -> 'a list -> 'a list*)
fold_right (fun (x,y) a -> fold_left (@) a y |> x)

(*'a list -> 'a list list*)
let f l = fold_right (fun x y -> [x]::y) l []

(*'a -> ('a -> int) -> int*)
let g x y z = (z y) * x in let f = g 5 in f
