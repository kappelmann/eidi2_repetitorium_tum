open Batteries
let todo _ = failwith "TODO"

module MyMap = struct
  (* type ('k,'v) t = ('k*'v) list (* this would not be very efficient *) *)
  (* implement map as binary trees, since we don't have functors yet, we split up the content and only compare on 'k *)
  type ('k,'v) t = Empty | Node of 'k * 'v * ('k,'v) t * ('k,'v) t

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
  let mem k m = todo ()

  (* return the smallest binding of the given map *)
  let min m = todo ()

  (* return the largest binding of the given map *)
  let max m = todo ()

  (* remove k m returns a map containing the same bindings as m, except the binding for k *)
  let remove k m = todo ()

  (* map f m returns a map with same domain as m, where the associated value a of all bindings of m has been replaced by the result of the application of f to a *)
  let map f m = todo ()

  (* filter f m returns the map with all the bindings in m that satisfy predicate f *)
  let filter f m = todo ()

  (* merge f m1 m2 computes a map whose keys are a subset of the union of keys of m1 and of m2. The presence of each such binding, and the corresponding value, is determined with the function f *)
  (* let a = from_list [1,"a";  2,"b"; 3,"c"] in
   * let b = from_list [1,1;    2,2;   4,4] in
   * then merge f a b contains the bindings f 1 (Some "a") (Some 1), f 2 (Some "b") (Some 2), f 3 (Some "c") None, f 4 None (Some 4) that return something *)
  let merge f a b = todo ()
end

module Json = struct
  (* our json datatype using maps and lists *)
  type t =
    | Null
    | Bool of bool
    | Number of float
    | String of string
    | Object of (string,t) MyMap.t
    | Array  of t list
  (* t is recursive! offset is used for accesssing inner json values *)
  type offset = Field of string | Index of int (* Field for Object, Index for Array *)
  (* we write foo.bar[1].baz for [Field "foo"; Field "bar"; Index 1' Field "baz"] *)
  type path = offset list

  (* example:
    { "a": null, "b": true, "c": 1.2, "d": "hello \"you\"!", "e": {  }, "f": [0, false, "no"] }
  *)
  let show json = todo ()

  (* map f over all values of type t *)
  let map f json = todo ()

  (* return the content of a Field or Index of a json value or None *)
  (* e.g. get (Index 1) (Array [Null, Bool true, Bool false]) = Some (Bool true)
   *      get_offset (Index 1) (Array []) = None
   *      get_offset (Index 1) Null = None
   *)
  let get_offset offset json = todo ()

  (* same as get_offset, but now on a whole path. if any part of the path fails we return None, otherwise the value. *)
  (* e.g. get [Field "a"; Index 0] (Object (MyMap.from_list ["a", Array [Null]])) = Some Null
   *      get [Field "a"; Index 1] (Object (MyMap.from_list ["a", Array [Null]])) = None
   *)
  let get path json = todo ()

  (* same as get_offset, but now we want to set some value *)
  (* e.g. set_offset (Field "a") Null (Object MyMap.empty) = Some (Object (MyMap.from_list ["a", Null]))
          set_offset (Index 1) (Bool true) (Array [Null; Null; Null] = Some (Array [Null; Bool true; Null]
          if the index is out of range we just return None:
          set_offset (Index 1) (Bool true) (Array [Null]) = None

   *)
  let set_offset offset v json = todo ()

  (* same as set_offset, but for setting a json value at some path *)
  (* e.g. set [Field "a"; Index 1] (Bool true) (Object (MyMap.from_list ["a", Array [Null; Null]])) = Some (Object (MyMap.from_list ["a", Array [Null; Bool true]]))
   *      set [Field "a"; Index 2] (Bool true) (Object (MyMap.from_list ["a", Array [Null; Null]])) = None
   *      set [Field "a"] (Bool true) (Object (MyMap.empty)) = Some (Object MyMap.(add "a" (Bool true) empty))
   *      set [] v json = Some v
   *)
  let set path value json = todo ()
end
