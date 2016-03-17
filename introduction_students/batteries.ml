let todo _ = failwith "TODO"
let (%) g f x = g (f x)
let comp2 f g x y = f (g x) (g y)
let compareBy f = comp2 compare f
let id x = x
let flip f x y = f y x
let neg f x = not (f x)
let const x _ = x

module Tuple2 = struct
  let cons x y = x,y
  let show f1 f2 (x,y) = "("^f1 x^", "^f2 y^")"
  let fst (x,y) = x
  let snd (x,y) = y
  let map1 f (x,y) = f x, y
  let map2 f (x,y) = x, f y
  let map f1 f2 (x,y) = f1 x, f2 y
  let swap (x,y) = y,x
  let curry f x y = f (x,y)
  let uncurry f (x,y) = f x y
end
let curry, uncurry = Tuple2.(curry, uncurry)

module Option = struct
  let some x = Some x (* Option monad's return *)
  let is_some x = x<>None
  let is_none x = x=None
  let get_some = function Some x -> x | None -> failwith "get_some got None"
  let show f = function
    | Some x -> "Some (" ^ f x ^ ")"
    | None -> "None"

  let map f = function
    | Some x -> Some (f x)
    | None -> None
  let map_default f x = function
    | Some v -> f v
    | None -> x

  (* if you have possibly failing functions f1, f2, f3 you can bind them together: f1 x >>= f2 >>= f3. why/when could this be useful? *)
  let bind o f = match o with
    | Some x -> f x
    | None -> None

  let filter p = function
    | Some x when p x -> Some x
    | _ -> None

  let default x = function
    | Some y -> y
    | None -> x

  let try_some f x = try Some (f x) with _ -> None

  module Infix = struct
    let (>>=) = bind
    let (|?) x y = default y x (* Some 1 |? 0 = 1, None |? 0 = 0 *)
  end
  include Infix
end
include Option.Infix

(* similar to BatteriesExceptionless.List *)
module List = struct
  let cons x xs = x :: xs
  let head = function [] -> None | x::_ -> Some x
  let tail = function [] -> None | _::xs -> Some xs
  let rec last = function [] -> None | [x] -> Some x | _::xs -> last xs
  let rec reverse = function [] -> [] | x::xs -> reverse xs @ [x]
  let rec length = function [] -> 0 | _::xs -> 1 + length xs
  let rec map f = function [] -> [] | x::xs -> f x :: map f xs
  let rec iter f = function [] -> () | x::xs -> let () = f x in iter f xs
  let mapi f = let rec g i = function [] -> [] | x::xs -> f i x :: g (i+1) xs in g 0
  let rec filter p = function [] -> [] | x::xs -> if p x then x :: filter p xs else filter p xs
  let rec fold_left f a = function [] -> a | x::xs -> fold_left f (f a x) xs
  let rec fold_right f = function [] -> (fun x -> x) | x::xs -> fun a -> f x (fold_right f xs a) (* not tail recursive! *)
  let exists p xs = fold_right ((||)%p) xs false
  let for_all p xs = fold_right ((&&)%p) xs true

  (* return Some element at index i or None if out of bounds. e.g. at 1 [1;2;3] = Some 2, at 1 [] = None *)
  let at i =
    let rec f j = function
      | [] -> None
      | x::xs -> if j=i then Some x else f (j+1) xs
    in f 0
  (* concatenate a list of lists. e.g. flatten [[1;2];[2;3]] = [1;2;2;3] *)
  let rec flatten = function
    | [] -> []
    | x::xs -> x @ flatten xs
  (* a list containing n elements x. e.g. make 3 1 = [1;1;1], make 2 'x' = ['x';'x'] *)
  let rec make n x = if n <= 0 then [] else x :: make (n-1) x
  (* range 1 3 = [1;2;3], range 1 (-1) = [1;0;-1] *)
  let rec range a b =
    let s x = if a<=b then x+1 else x-1 in
    if a=b then [a]
    else a :: range (s a) b
  (* [f 0; f 1; ...; f (n-1)]. e.g. init 3 (fun x -> Char.chr (x+65)) = ['A'; 'B'; 'C'], [] for n<1 *)
  let init n f = if n<1 then [] else map f (range 0 (n-1))
  (* reduce f [] = None, reduce f [1] = Some 1, reduce f [1;2;3] = Some (f (f 1 2) 3) *)
  let reduce f = function [] -> None | x::xs -> Some (fold_left f x xs)
  (* maximum element of a list. e.g. max_el [2;1;3] = Some 3, max_el [] = None *)
  let max_el xs = reduce max xs
  (* let rec max_el = function [] -> None | x::xs -> let m = max_el xs in if Some x>m then Some x else m *)
  (* min_max [] = None, min_max [2;4;1;2] = (1,4) *)
  let min_max xs = fold_right (fun x -> function Some (l,h) -> Some (min l x, max h x) | _ -> Some (x,x)) xs None
  (* mem 2 [1;2;3] = true; mem 2 [1;3] = false *)
  let rec mem e = function
    | [] -> false
    | x::xs -> x=e || mem e xs (* short-circuits :) *)
  (* find even [1;2;3;4] = Some 2, find even [1;3] = None *)
  let rec find p = function
    | [] -> None
    | x::xs -> if p x then Some x else find p xs
  (* filter_map (fun x -> if even x then Some (x*2) else None) [1;2;3;4] = [4;8], filter_map (const None) [1;2;3] = [] *)
  let rec filter_map f = function
    | [] -> []
    | x::xs -> (match f x with Some x -> cons x | None -> id) (filter_map f xs)
  (* partition even [1;2;3;4] = ([2;4],[1;3]) *)
  let rec partition p xs = fold_right (fun x (yay,nay) -> if p x then x::yay,nay else yay,x::nay) xs ([],[])
  (* index_of 2 [1;2;3] = Some 1, index_of 2 [] = None *)
  let index_of e =
    let rec f i = function
      | [] -> None
      | x::xs -> if x=e then Some i else f (i+1) xs
    in f 0
  (* split_at 1 [1;2;3] = ([1],[2;3]), split_at 2 [1] = ([1],[]) *)
  let split_at i =
    let rec f j = function
      | [] -> [],[]
      | x::xs -> Tuple2.(if j<i then map1 else map2) (cons x) (f (j+1) xs)
    in f 0
  (* remove_all even [1;2;3;4] = [1;3] *)
  let remove_all p = filter (neg p)
  (* remove_first even [1;2;3;4] = [1;3;4] *)
  let rec remove_first p = function
    | [] -> []
    | x::xs -> if p x then xs else x :: remove_first p xs
  (* take 2 [1;2;3;4] = [1;2], take 2 [] = [] *)
  let take n =
    let rec f i = function
      | x::xs when i<n -> x :: f (i+1) xs
      | _ -> []
    in f 0
  (* drop 2 [1;2;3;4] = [3;4], drop 2 [] = [] *)
  let drop n =
    let rec f i = function
      | x::xs when i<n -> f (i+1) xs
      | xs -> xs
    in f 0
  (* interleave 0 [1;2;3] = [1;0;2;0;3], interleave 0 [] = [], interleave 0 [1] = [1] *)
  let interleave e xs = match fold_right (fun x a -> e::x::a) xs [] with [] -> [] | x::xs -> xs
  (* split [1,2;2,3;3,4] = ([1;2;3],[2;3;4]), split [] = ([],[]) *)
  let rec split = function
    | [] -> [],[]
    | (x,y)::xs -> let a,b = split xs in x::a,y::b
  (* combine [1;2] [3;4] = Some [1,3;2,4], combine [1] [2;3] = None *)
  let rec combine a b = match a,b with
    | [],[] -> Some []
    | x::xs,y::ys -> (match combine xs ys with Some xs -> Some ((x,y) :: xs) | None -> None)
    | _ -> None

  let rec merge ?(cmp=compare) = curry @@ function
    | xs, [] | [], xs -> xs
    | h1::t1, h2::t2 -> if cmp h1 h2 < 0 then h1 :: merge ~cmp:cmp t1 (h2::t2) else h2 :: merge ~cmp:cmp (h1::t1) t2
  let rec sort ?(cmp=compare) = function
    | [] -> []
    | [x] -> [x]
    | xs ->
        let xs, ys = split_at (length xs / 2) xs in
        merge ~cmp:cmp (sort ~cmp:cmp xs) (sort ~cmp:cmp ys)

  let show s xs = "[" ^ String.concat ", " (map s xs) ^ "]"
  let flat_map f xs = flatten @@ map f xs
  let find_map p l = fold_left (fun a x -> match a with Some e -> Some e | None -> p x) None l
  let choose = function [] -> None | x :: _ -> Some x
end

module Set = struct
  type 'a t = 'a list (* this should be abstract in the interface so that one has to use {from,to}_list *)

  (* of_list [1;2;1] = [1;2] *)
  let rec of_list l : 'a t =
    let rec doit = function
    | [] -> []
    | x::xs -> x :: doit (List.filter ((<>) x) xs) in
    List.sort l |> doit
  let to_list x = x
  (* union [1;2] [2;3] = [1;2;3] *)
  let union a b = of_list (a@b)
  (* inter [1;2] [2;3] = [2] *)
  let inter a b = List.(union (filter (flip mem a) b) (filter (flip mem b) a))
  (* diff [1;2;3] [1;3] = [2] *)
  let diff a b = List.(filter (neg (flip mem b)) a)
  let fold f x a = to_list a |> List.fold_right f x
  let map f x = to_list x |> List.map f |> of_list
  let cardinal x = to_list x |> List.length
  let is_empty x = cardinal x = 0
  let mem x s = to_list s |> List.mem x
  let for_all p s = to_list s |> List.for_all p
  let exists p s = to_list s |> List.exists p
  let remove x s = to_list s |> List.filter (fun y -> y <> x) |> of_list
  let choose s = to_list s |> List.choose
  let add x s = to_list s |> List.cons x |> of_list
  let empty = of_list []
  let find f = List.find f % to_list
end

module String = struct
  let concat x xs = List.interleave x xs |> List.fold_left (^) ""
  let escaped x = x (* don't care for now *)
  let explode s = List.init (String.length s) (fun i -> s.[i])
  let implode l = String.init (List.length l) (Option.get_some % flip List.at l)
    (*let res = String.create (List.length l) in
    let rec implode' i = function
      | [] -> res
          | c :: l -> res.[i] <- c; implode' (i + 1) l
    in implode' 0 l*)
  let cons x xs = implode (x::explode xs) (* this is not very efficient... *)
  let of_char x = implode [x] (* same *)
end

module Map = struct
  (* type ('k,'v) t = ('k*'v) list (* this would not be very efficient *) *)
  (* implement map as binary trees, since we don't have functors yet, we split up the content and only compare on 'k *)
  type ('k,'v) t = Empty | Node of 'k * 'v * ('k,'v) t * ('k,'v) t

  open Option

  let empty = Empty

  let rec to_list = function
    | Empty -> []
    | Node (k, v, l, r) -> (k,v) :: to_list l @ to_list r

  let show sk sv m = to_list m |> List.map (fun (k,v) -> sk k ^ " -> " ^ sv v) |> String.concat ", " |> fun s -> "{ " ^ s ^ " }"

  let rec add k' v' = function
    | Empty -> Node (k', v', Empty, Empty)
    | Node (k, v, l, r) ->
        let v,l,r = if k'=k then v',l,r else if k'<k then v, add k' v' l, r else v, l, add k' v' r in
        Node (k, v, l, r)

  let of_list xs = List.fold_left (fun m (k,v) -> add k v m) empty xs

  let rec find k = function
    | Empty -> None
    | Node (k', v', l, r) ->
        if k = k' then Some v'
        else find k (if k < k' then l else r)

  (* mem k m returns true if m contains a binding for k, and false otherwise *)
  let mem k = is_some % find k

  (* remove k m returns a map containing the same bindings as m, except the binding for k *)
  let remove k m = to_list m |> List.remove_first ((=) k % fst) |> of_list

  (* return the smallest binding of the given map *)
  let rec min = function
    | Empty -> None
    | Node (k, v, x, _) -> Some (min x |? (k,v))

  (* return the largest binding of the given map *)
  let rec max = function
    | Empty -> None
    | Node (k, v, _, x) -> Some (max x |? (k,v))

  (* remove k m returns a map containing the same bindings as m, except the binding for k *)
  let rec remove k = function
    | Empty -> Empty
    | Node (k', v', Empty, Empty) when k=k' -> Empty
    | Node (k', v', Empty, x) when k=k' -> x
    | Node (k', v', x, Empty) when k=k' -> x
    | Node (k', v', l, r) when k=k' -> let lk,lv = get_some (max l) in Node (lk, lv, remove lk l, r)
    | Node (k', v', l, r) -> Node (k', v', remove k l, remove k r)

  (* map f m returns a map with same domain as m, where the associated value a of all bindings of m has been replaced by the result of the application of f to a *)
  let rec map f = function
    | Empty -> Empty
    | Node (k, v, l, r) -> Node (k, f v, map f l, map f r)

  (* filter f m returns the map with all the bindings in m that satisfy predicate f *)
  let rec filter f m = to_list m |> List.filter f |> of_list

  (* merge f m1 m2 computes a map whose keys are a subset of the union of keys of m1 and of m2. The presence of each such binding, and the corresponding value, is determined with the function f *)
  (* let a = of_list [1,"a";  2,"b"; 3,"c"] in
   * let b = of_list [1,1;    2,2;   4,4] in
   * then merge f a b contains the bindings f 1 (Some "a") (Some 1), f 2 (Some "b") (Some 2), f 3 (Some "c") None, f 4 None (Some 4) that return something *)
  let rec merge f a b =
    let open Option in
    let la = to_list a |> List.filter_map (fun (k,v) -> f k (Some v) (find k b) >>= fun v' -> Some (k,v')) in
    let lb = to_list b |> List.filter_map (fun (k,v) -> f k (find k a) (Some v) >>= fun v' -> Some (k,v')) in
    of_list (la @ lb)
end
