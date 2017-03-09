open Batteries


module Json = struct
  include Ha5.Json

  (* homework *)
  (* shows the path: we write .foo.bar[1].baz for [Field "foo"; Field "bar"; Index 1; Field "baz"] *)
  let rec show_path = function
    | [] -> ""
    | x::xs -> (match x with Field s -> "."^s | Index i -> "["^string_of_int i^"]") ^ show_path xs

  (* gets the children, i.e., the values of Object and Array *)
  let get_children = function
    | Object x -> Map.to_list x |> List.map snd
    | Array x -> x
    | _ -> []

  (* gets all subtrees that match the path (path does not have to start at the root).
   * e.g. get_all [Field "foo"] (Array [Null; Object (Map.from_list ["foo", String "one"; "bar", Null]); Object (Map.from_list ["baz", Null; "foo", String "two"])]) = [String "one"; String "two"] *)
  let rec get_all path json =
    Option.map_default List.cons id (get path json) @@ List.flat_map (get_all path) (get_children json)

  (* parse string and return some json if there are no errors *)
  let from_string s =
    (* let i = String.explode s in *)
    (* let open Parser in *)
    (* (* let pstr = between (char '"') (many any) (char '"') in *) *)
    (* let pjson = many lower >>= fun xs -> result (String.implode xs) in *)
    (* pjson i *)
    Some (String s)

  let rec from_string s =
    let open String in let open Option in
    (* input -> 'a option: these parsers consume the whole input *)
    let null = function "null" -> Some Null | _ -> None in
    let bool = try_some bool_of_string %> map (fun x -> Bool x) in (* no curried constructors :( *)
    let num = try_some int_of_string %> map (fun x -> Number x) in
    let between a b = explode %> fun c -> List.(if head c = Some a && last c = Some b then Some (crop c |> chars trim) else None) in
    (* input -> ('a * input) option: these parsers may succeed with some rest input *)
    let longest_prefix f xs = List.(find_map f (reverse (inits xs))) in
    let parse c = longest_prefix (from_string%implode) c |> map (fun x -> x, List.drop (List.length c) c) in
    let drop c = function x,r::rs when r=c -> Some (x,rs) | x,[] -> Some (x,[]) | _ -> None in
    let rec parr = function
      | [] -> Some []
      | c -> parse c >>= drop ',' >>= fun (x,r) -> parr r >>= fun xs -> Some (x::xs)
    in
    let rec pobj = function
      | [] -> Some []
      | c -> parse c >>= drop ':' >>= function String k, r -> parse r >>= drop ',' >>= fun (v,r) -> pobj r >>= fun xs -> Some ((k,v)::xs) | _ -> None
    in
    let str = between '"' '"' %> filter (neg @@ List.mem '"') %> map (fun x -> String (implode x)) in
    let arr s = between '[' ']' s >>= parr %> map (fun x -> Array x) in
    let obj s = between '{' '}' s >>= pobj %> map (fun x -> Object (Map.from_list x)) in
    let (|||) p q s = match p s with None -> q s | x -> x in (* try parsers from left to right *)
    trim s |> (null ||| bool ||| num ||| str ||| obj ||| arr)
end

let () =
  let open Json in
  (* let t p o = *)
  (*   print_endline @@ "path: " ^ show_path p ^ ", object: " ^ (show o); *)
  (*   print_endline @@ List.show show @@ get_all p o *)
  (* in *)
  (* t [Field "a"; Index 1] (Object Map.(from_list ["a", Array [Null; Null]])); *)
  (* t [Field "a"] (Object Map.empty); *)
  (* t [Field "foo"] (Array [Null; Object (Map.from_list ["foo", String "one"; "bar", Null]); Object (Map.from_list ["baz", Null; "foo", String "two"])]); *)
  let t s =
    print_endline s;
    print_endline @@ Option.show show (from_string s)
  in
  (* t "{ \"a\": null, \"b\": true, \"c\": 1.2, \"d\": \"hello you!\", \"e\": {  }, \"f\": [0, false, \"no\"] }"; *)
  t " null";
  t "false";
  t "\"huhu\" ";
  t "[]";
  t "{}";
  t "[null]";
  t "{\"a\":null}";
  t "[null,false,null]";
  t "{ \"a\":null,\"ab\":false}";
