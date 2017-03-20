open Kaputt.Abbreviations
open Ha11_angabe
open Batteries.List

module X = Ha11_dummy

let tests = ref []
let points = ref []
let add points_test test =
  points := !points @ [points_test];
  tests := !tests @ [test]

let shuffle d =
  let nd = List.rev_map (fun c -> (Random.bits (), c)) d in
  let sond = List.sort (fun (x, _) (y, _) -> Pervasives.compare x y) nd in
  List.rev_map snd sond

let test_map (type a) data (module M : Ha11_angabe.Map with type key = a) =
  let length = List.length data in
  let shuffled = shuffle data in
  let (lower, higher, _) = List.fold_left (fun (l, h, i) next ->
      if i < length / 2 then (next::l, h, i + 1)
      else (l, next::h, i + 1)) ([], [], 0) shuffled in
  let m = List.fold_left (fun map (k, v) -> M.insert map k v) (M.create ()) lower in
  Assert.equal ~msg:"size after insert" (M.size m) (List.length lower);
  let lower = shuffle lower in
  let m = List.fold_left (fun map (k, v) -> M.insert map k v) m lower in
  Assert.equal ~msg:"size after insertion of already present elements"
    (M.size m) (List.length lower);
  let m = List.fold_left (fun map (k, _) -> M.remove map k) m higher in
  Assert.equal ~msg:"size after removal of non-present elements"
    (M.size m) (List.length lower);
  let lower = shuffle lower in
  List.iter (fun (k, v) -> Assert.equal ~msg:"lookup present element"
                (Some v) (M.lookup m k)) lower;
  List.iter (fun (k, v) -> Assert.equal ~msg:"lookup non-present element"
                None (M.lookup m k)) higher;
  let _ = List.fold_left (fun (map, length) (k, v) ->
      Assert.equal ~msg:"lookup present element"
        (Some v) (M.lookup map k);
      let l' = length - 1 in
      let m' = M.remove map k in
      Assert.equal ~msg:"size after remove" l' (M.size m');
      Assert.equal ~msg:"lookup removed element" None (M.lookup m' k);
      (m', l')) (m, List.length lower) lower in ()

let from_int_int x = (x, 2*x + 1)

let from_string_float x =
  (String.concat "q" ["p"; string_of_float (42.3 +. 7.2*.float_of_int x);
                      string_of_int x], 2.7*.(float_of_int x) -. 9.1)

let gen_data f =
  let n = (Random.int 200) + 100 in
  init n f

let test_makehashmap (type a) f =
  let (module HashMap) = (module X.MakeHashMap (struct
        type key = a
        let hash = Hashtbl.hash
      end) : Ha11_angabe.Map with type key = a) in
  test_map (gen_data f) (module HashMap)

let test_makehashmap () =
  test_makehashmap from_int_int;
  test_makehashmap from_string_float

let test_int_hashmap () =
  test_map (gen_data from_int_int) (module X.IntHashMap)

let test_maketreemap (type a) f =
  let (module TreeMap) = (module X.MakeTreeMap (struct
        type key = a
        let compare = Pervasives.compare
      end) : Ha11_angabe.Map with type key = a) in
  test_map (gen_data f) (module TreeMap)

let test_maketreemap () =
  test_maketreemap from_int_int;
  test_maketreemap from_string_float

let test_int_treemap () =
  test_map (gen_data from_int_int) (module X.IntTreeMap)

let from_int x = -20*x + 2*x*x + 2

let check_preorder (type a) l =
  let _ = List.fold_left (fun (lb, stack) x ->
      (match lb with 
         Some bound -> Assert.is_true ~msg:"pre-order" (x >= bound)
       | None -> ());
      let rec pop lb stack =
        (match stack with
           hd::tl when hd < x -> pop (Some hd) tl
         | hd::tl as stack -> (lb, stack)
         | _ -> (lb, [])) in
      let (lb, stack) = pop lb stack in
      (lb, stack)
    ) (None, []) l in ()

type eq = { f : 'a. ?msg:string -> 'a list -> 'a list -> unit }

let test_extended eq data (module M : Ha11_angabe.Extended) =
  let m = List.fold_right (fun x ext -> M.insert x ext) data M.empty in
  let l = ref [] in
  M.iter (fun x -> l := x::!l) m;
  eq.f ~msg:"iter" data (List.rev !l);
  let f x = 4.2*.(float_of_int x) -. 3.1 in
  let mapped = List.map f data in
  let mapped_m = M.map f m in
  eq.f ~msg:"map" mapped (M.to_list mapped_m);
  let f x = x < 0 in
  let filtered = List.filter f data in
  let filtered_m = M.filter f m in
  eq.f ~msg:"filter" filtered (M.to_list filtered_m);
  let appended = List.append data filtered in
  let appended_m = M.append m filtered_m in
  eq.f ~msg:"append" appended (M.to_list appended_m);
  let concatted = [data; List.map int_of_float mapped; filtered; appended] in
  let concatted_m = M.insert m (M.insert (M.map int_of_float mapped_m)
                                  (M.insert filtered_m (M.insert appended_m M.empty))) in
  let flattened = List.flatten concatted in
  let flattened_m = M.flatten concatted_m in
  eq.f ~msg:"flatten" flattened (M.to_list flattened_m);
  eq.f ~msg:"to_list" data (M.to_list m);
  eq.f ~msg:"of_list" data (M.to_list (M.of_list data))

let test_extended_tree (module M : Ha11_angabe.Extended) =
  test_extended {f = (fun ?msg l ext ->
      check_preorder ext;
      let sorted = List.sort Pervasives.compare l in
      let sorted_m = List.sort Pervasives.compare ext in
      Assert.equal ?msg sorted sorted_m)} (gen_data from_int) (module M)

let test_extended_tree () =
  test_extended_tree (module X.ExtendedSearchTree);
  test_extended_tree (module (X.Lift (X.SearchTree)))

let test_extended_list (module M : Ha11_angabe.Extended) =
  test_extended {f = (fun ?msg l ext -> Assert.equal ?msg l ext)}
    (gen_data from_int) (module M)

let test_extended_list () =
  test_extended_list (module X.ExtendedList);
  test_extended_list (module (X.Lift (X.List)))

let () =
  add 40 @@ Test.make_simple_test ~title:"test_makehashmap" test_makehashmap;
  add 10 @@ Test.make_simple_test ~title:"test_int_hashmap" test_int_hashmap;
  add 40 @@ Test.make_simple_test ~title:"test_maketreemap" test_maketreemap;
  add 10 @@ Test.make_simple_test ~title:"test_int_treemap" test_int_treemap;
  add 40 @@ Test.make_simple_test ~title:"test_extended_list" test_extended_list;
  add 60 @@ Test.make_simple_test ~title:"test_extended_tree" test_extended_tree

(* launch *)
let () =
  Random.self_init ();
  let point = Test.(function
      | Passed -> 1
      | Report (p,n,e,c,m) when p = n -> 1
      | _ -> 0)
  in
  let passed = List.map point (Test.exec_tests !tests) in
  Test.run_tests ~output:(Test.Html_output (open_out "result.html")) !tests;
  Test.run_tests !tests;
  prerr_endline @@ "### GRADES: " ^ String.concat " "
  @@ List.map2 (fun x y -> string_of_int (y*x)) passed !points
