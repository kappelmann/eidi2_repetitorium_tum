open Kaputt.Abbreviations
open Ha10_angabe
open Batteries.List

module X = Ha10

let tests = ref []
let points = ref []
let add points_test test =
  points := !points @ [points_test];
  tests := !tests @ [test]

let shuffle d =
  let nd = List.rev_map (fun c -> (Random.bits (), c)) d in
  let sond = List.sort (fun (x, _) (y, _) -> Pervasives.compare x y) nd in
  List.rev_map snd sond

let test_map (module M : Ha10_angabe.Map) () =
  let ki = {
    compare = Pervasives.compare;
    hash = Hashtbl.hash
  } in
  let n = (Random.int 30) + 10 in
  let numbers_sorted = init n (fun x -> x) in
  let m = List.fold_left (fun map i -> M.insert map i (2*i + 1)) (M.create ki) numbers_sorted in
  Assert.equal (M.size m) n;
  let shuffled = shuffle numbers_sorted in
  let assert_contains m x =
    let size = M.size m in
    let m' = M.insert m x 0 in
    let size' = M.size m' in
    Assert.equal size size' in
  let assert_not_contains m x =
    let size = M.size m in
    let m' = M.insert m x 0 in
    let size' = M.size m' in
    Assert.equal (size + 1) size';
    let m'' = M.remove m' x in
    let size'' = M.size m'' in
    Assert.equal size size'' in
  List.iter (fun x -> assert_contains m x) shuffled;
  for i = n + 27 to n + 30 do
    assert_not_contains m i
  done;
  let m' = List.fold_left (fun m x ->
      assert_contains m x;
      let m' = M.remove m x in
      assert_not_contains m' x; m')
      m shuffled in
  Assert.equal (M.size m') 0

let test_tutor () =
  Assert.fail_msg "Your tutor needs to check your code."

let test_vector () =
  for i = 0 to 50 do
    let n = (Random.int 10) + 10 in
    let create_vector () =
      let next = ref 0 in
      let indices = init n (fun i ->
          let v = !next + (Random.int 2) in
          next := v;
          v
        ) in
      let indices_shuffled = shuffle indices in
      List.fold_left (fun v i ->
          let value = Random.int 10 in
          let v' = X.SparseVector.set i value v in
          v'
        ) X.SparseVector.empty indices_shuffled in
    let x = 
      X.SparseVector.sprod
        (X.SparseVector.add (create_vector ())
           (X.SparseVector.mul (Random.int 5) (create_vector ())
           )) (create_vector ()) in
    (* Nur ein Template; hier richtigen Wert einsetzen, z.B. aus
       Papierrechnung *)
    Assert.equal x 42
  done

let () =
  add 0 @@ Test.make_simple_test ~title:"test_hashmap_basic" (test_map (module X.HashMap));
  add 0 @@ Test.make_simple_test ~title:"test_hashmap" test_tutor;
  add 0 @@ Test.make_simple_test ~title:"test_treemap_basic" (test_map (module X.TreeMap));
  add 0 @@ Test.make_simple_test ~title:"test_hashmap" test_tutor;
  add 0 @@ Test.make_simple_test ~title:"test_vector_basic" test_vector;
  add 0 @@ Test.make_simple_test ~title:"test_vector" test_tutor

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
