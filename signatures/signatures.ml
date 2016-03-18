open List

fold_left (fun a b -> b a)

fold_right (fun b a -> b a)

fold_left (fun a b -> b)

fold_left (fun a b -> a)

filter (fun a -> a<>None)

filter (fun a -> a 7)

let f x = match x with (x,y) -> x y

fold_left (fun a b -> a @ (filter (fun b -> b="") b)) []

(*unit -> Thread.t*)
let f x = Thread.create (fun x -> new_channel x) x

let a f x = f x |> reverse

let a f x = f(f x)

let a = fold_left (fun a x -> x) (0)

map (fun _ -> ()) [1;2;3]

let x = "unwichtig" in map (fun _ -> (x)) [1;2;3]

let x = "wichtig" in map (fun x -> (x)) [1;2;3]

fold_right (fun (x,y) a -> fold_left (@) a y)

fold_right (fun (x,y) a -> fold_left (@) a y |> x)

let f l = fold_right (fun x y -> [x]::y) l []

let g x y z = (z y) * x in let f = g 5 in f
