open Batteries

module MyMap = struct
  open Ha4.MyMap

  open Option

  let empty = Empty

  let show sk sv m =
    let sp k v = sk k ^ " -> " ^ sv v in
    let (|^) a b = if a <> "" then if b <> "" then a ^ ", " ^ b else a else b in
    let rec f = function
      | Empty -> ""
      | Node (k, v, l, r) -> sp k v |^ f l |^ f r
    in
    "{ " ^ f m ^ " }"

  let rec to_list = function
    | Empty -> []
    | Node (k, v, l, r) -> (k,v) :: to_list l @ to_list r

  let show sk sv m = to_list m |> List.map (fun (k,v) -> sk k ^ " -> " ^ sv v) |> String.concat ", " |> fun s -> "{ " ^ s ^ " }"

  let rec add k' v' = function
    | Empty -> Node (k', v', Empty, Empty)
    | Node (k, v, l, r) ->
        let v,l,r = if k'=k then v',l,r else if k'<k then v, add k' v' l, r else v, l, add k' v' r in
        Node (k, v, l, r)

  let from_list xs = List.fold_left (fun m (k,v) -> add k v m) empty xs

  let rec find k = function
    | Empty -> None
    | Node (k', v', l, r) ->
        if k = k' then Some v'
        else find k (if k < k' then l else r)

  (* homework *)
  (* mem k m returns true if m contains a binding for k, and false otherwise *)
  let mem k = is_some % find k

  (* remove k m returns a map containing the same bindings as m, except the binding for k *)
  let remove k m = to_list m |> List.remove_first ((=) k % fst) |> from_list

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
  let rec filter f m = to_list m |> List.filter f |> from_list

  (* merge f m1 m2 computes a map whose keys are a subset of the union of keys of m1 and of m2. The presence of each such binding, and the corresponding value, is determined with the function f *)
  (* let a = from_list [1,"a";  2,"b"; 3,"c"] in
   * let b = from_list [1,1;    2,2;   4,4] in
   * then merge f a b contains the bindings f 1 (Some "a") (Some 1), f 2 (Some "b") (Some 2), f 3 (Some "c") None, f 4 None (Some 4) that return something *)
  let rec merge f a b =
    let open Option in
    let la = to_list a |> List.filter_map (fun (k,v) -> f k (Some v) (find k b) >>= fun v' -> Some (k,v')) in
    let lb = to_list b |> List.filter_map (fun (k,v) -> f k (find k a) (Some v) >>= fun v' -> Some (k,v')) in
    from_list (la @ lb)
end

module Json = struct
  open Ha4.Json

  (* example:
    { "a": null, "b": true, "c": 1.2, "d": "hello \"you\"!", "e": {  }, "f": [0, false, "no"] }
  *)
  let rec show = function
    | Null -> "null"
    | Bool x -> string_of_bool x
    | Number x -> string_of_float x
    | String x -> "\"" ^ String.escaped x ^ "\""
    | Object x -> "{ " ^ String.concat ", " (List.map (fun (k,v) -> show (String k) ^ ": " ^ show v) (MyMap.to_list x)) ^ " }"
    | Array x -> "[" ^ String.concat ", " (List.map show x) ^ "]"

  (* map f over all values of type t *)
  let rec map f = function
    | Object x -> f (Object (MyMap.map (map f) x))
    | Array x -> f (Array (List.map (map f) x))
    | x -> f x

  (* return the content of a Field or Index of a json value or None *)
  (* e.g. get (Index 1) (Array [Null, Bool true, Bool false]) = Some (Bool true)
   *      get_offset (Index 1) (Array []) = None
   *      get_offset (Index 1) Null = None
   *)
  let get_offset offset json = match offset, json with
    | Field f, Object o -> MyMap.find f o
    | Index i, Array a -> List.at i a
    | _ -> None

  (* same as get_offset, but now on a whole path. if any part of the path fails we return None, otherwise the value. *)
  (* e.g. get [Field "a"; Index 0] (Object (MyMap.from_list ["a", Array [Null]])) = Some Null
   *      get [Field "a"; Index 1] (Object (MyMap.from_list ["a", Array [Null]])) = None
   *)
  let rec get path json =
    match path with
    | [] -> Some json
    | x::xs -> Option.(get_offset x json >>= get xs)

  (* same as get_offset, but now we want to set some value *)
  (* e.g. set_offset (Field "a") Null (Object MyMap.empty) = Some (Object (MyMap.from_list ["a", Null]))
          set_offset (Index 1) (Bool true) (Array [Null; Null; Null] = Some (Array [Null; Bool true; Null]
          if the index is out of range we just return None:
          set_offset (Index 1) (Bool true) (Array [Null]) = None

   *)
  let set_offset offset v json = match offset, json with
    | Field f, Object o -> Some (Object (MyMap.add f v o))
    | Index i, Array a when i < List.length a -> Some (Array (List.mapi (fun j x -> if j=i then v else x) a))
    | _ -> None

  (* same as set_offset, but for setting a json value at some path *)
  (* e.g. set [Field "a"; Index 1] (Bool true) (Object (MyMap.from_list ["a", Array [Null; Null]])) = Some (Object (MyMap.from_list ["a", Array [Null; Bool true]]))
   *      set [Field "a"; Index 2] (Bool true) (Object (MyMap.from_list ["a", Array [Null; Null]])) = None
   *      set [Field "a"] (Bool true) (Object (MyMap.empty)) = Some (Object MyMap.(add "a" (Bool true) empty))
   *      set [] v json = Some v
   *)
  let rec set path value json =
    let open Option in
    match path with
    | [] -> Some value
    | [x] -> set_offset x value json
    | x::xs -> get_offset x json >>= set xs value >>= fun v -> set_offset x v json
end

(*
let () =
  let open Json in
  let t o p =
    print_endline @@ show o;
    print_endline (match set p (Bool true) o with Some x -> show x | _ -> "None")
  in
  t (Object MyMap.(from_list ["a", Array [Null; Null]])) [Field "a"; Index 1];
  t (Object MyMap.empty) [Field "a"];
*)
