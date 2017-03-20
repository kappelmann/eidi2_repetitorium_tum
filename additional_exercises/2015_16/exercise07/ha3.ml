let todo _ = failwith "TODO"

(* shortcuts *)
let (%) g f x = g (f x)
let id x = x
let flip f x y = f y x
let neg f x = not (f x)
module Tuple2 = struct
  let fst (x,y) = x
  let snd (x,y) = y
  let map1 f (x,y) = f x, y
  let map2 f (x,y) = x, f y
end

module MyList = struct
  let cons x xs = x :: xs
  let rec length = function [] -> 0 | _::xs -> 1 + length xs
  let rec map f = function [] -> [] | x::xs -> f x :: map f xs
  let rec filter p = function [] -> [] | x::xs -> if p x then x :: filter p xs else filter p xs
  let rec fold_left f a = function [] -> a | x::xs -> fold_left f (f a x) xs
  let rec fold_right f = function [] -> (fun x -> x) | x::xs -> fun a -> f x (fold_right f xs a) (* not tail recursive! *)

  (* return Some element at index i or None if out of bounds. e.g. at 1 [1;2;3] = Some 2, at 1 [] = None *)
  let at i xs = todo ()
  (* concatenate a list of lists. e.g. flatten [[1];[2;3]] = [1;2;3] *)
  let flatten xs = todo ()
  (* a list containing n elements x. e.g. make 3 1 = [1;1;1], make 2 'x' = ['x';'x'] *)
  let make n x = todo ()
  (* range 1 3 = [1;2;3], range 1 (-1) = [1;0;-1] *)
  let range a b = todo ()
  (* [f 0; f 1; ...; f (n-1)]. e.g. init 3 (fun x -> Char.chr (x+65)) = ['A'; 'B'; 'C'], [] for n<1 *)
  let init n f = todo ()
  (* reduce f [] = None, reduce f [1] = Some 1, reduce f [1;2;3] = Some (f (f 1 2) 3) *)
  let reduce f xs = todo ()
  (* maximum element of a list. e.g. max_el [2;1;3] = Some 3, max_el [] = None *)
  let max_el xs = todo ()
  (* min_max [] = None, min_max [2;4;1;2] = (1,4) *)
  let min_max xs = todo ()
  (* mem 2 [1;2;3] = true; mem 2 [1;3] = false *)
  let mem e xs = todo ()
  (* find even [1;2;3] = Some 2, find even [1;3] = None *)
  let find p xs = todo ()
  (* filter_map (fun x -> if even x then Some (x*2) else None) [1;2;3;4] = [4;8], filter_map (const None) [1;2;3] = [] *)
  let filter_map f xs = todo ()
  (* partition even [1;2;3;4] = ([2;4],[1;3]) *)
  let partition p s = todo ()
  (* index_of 2 [1;2;3] = Some 1, index_of 2 [] = None *)
  let index_of e xs = todo ()
  (* split_at 1 [1;2;3] = ([1],[2;3]), split_at 2 [1] = ([1],[]) *)
  let split_at i xs = todo ()
  (* remove_all even [1;2;3;4] = [1;3] *)
  let remove_all p xs = todo ()
  (* remove_first even [1;2;3;4] = [1;3;4] *)
  let remove_first p xs = todo ()
  (* take 2 [1;2;3;4] = [1;2], take 2 [] = [] *)
  let take n xs = todo ()
  (* drop 2 [1;2;3;4] = [3;4], drop 2 [] = [] *)
  let drop n xs = todo ()
  (* interleave 0 [1;2;3] = [1;0;2;0;3], interleave 0 [] = [] *)
  let interleave e xs = todo ()
  (* split [1,2;2,3;3,4] = ([1;2;3],[2;3;4]), split [] = ([],[]) *)
  let split xs = todo ()
  (* combine [1;2] [3;4] = Some [1,3;2,4], combine [1] [2;3] = None *)
  let combine a b = todo ()
end

module MySet = struct
  type 'a t = 'a list (* this is abstract in the interface so that one has to use {from,to}_list *)

  (* from_list [1;2;1] = [1;2] *)
  let from_list xs = todo ()
  let to_list x = x
  (* union [1;2] [2;3] = [1;2;3] *)
  let union a b = todo ()
  (* inter [1;2] [2;3] = [2] *)
  let inter a b = todo ()
  (* diff [1;2;3] [1;3] = [2] *)
  let diff a b = todo ()
end
