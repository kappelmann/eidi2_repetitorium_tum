open Kaputt.Abbreviations
open Ha10_angabe
open Batteries.List

module X = Ha10
module Sol = Ha10_sol

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
  let n = (Random.int 200) + 100 in
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
    (*Printf.printf "size = %d, size' = %d, size'' = %d\n" size size' size'';*)
    Assert.equal size size'' in
  List.iter (fun x -> assert_contains m x) shuffled;
  for i = n + 10 to n + 100 do
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

type vec = (int * int) list
[@@deriving show]

let test_vector () =
  let for_n n =
    let create_vectors () =
      let next = ref 0 in
      let indices = init n (fun i ->
          let v = !next + (Random.int 2) in
          next := v;
          v
        ) in
      let indices_shuffled = shuffle indices in
      List.fold_left (fun (raw, v, v_sol) i ->
          let value = Random.int 10 in
          let v' = X.SparseVector.set i value v in
          let v_sol' = Sol.SparseVector.set i value v_sol in
          let raw = (i, value)::(List.remove_assoc i raw) in
          (raw, v', v_sol')
        ) ([], X.SparseVector.empty, Sol.SparseVector.empty) indices_shuffled in
    let add (v1, v1_sol) (v2, v2_sol) =
      (X.SparseVector.add v1 v2, Sol.SparseVector.add v1_sol v2_sol) in
    let mul r (v, v_sol) =
      (X.SparseVector.mul r v, Sol.SparseVector.mul r v_sol) in
    let sprod (v1, v1_sol) (v2, v2_sol) =
      (X.SparseVector.sprod v1 v2, Sol.SparseVector.sprod v1_sol v2_sol) in
    let (a_raw, a, a_sol) = create_vectors () in
    let (b_raw, b, b_sol) = create_vectors () in
    let (c_raw, c, c_sol) = create_vectors () in
    let (d_raw, d, d_sol) = create_vectors () in
    let a_raw = List.sort (fun (x, _) (y, _) -> Pervasives.compare x y) a_raw in
    let b_raw = List.sort (fun (x, _) (y, _) -> Pervasives.compare x y) b_raw in
    let c_raw = List.sort (fun (x, _) (y, _) -> Pervasives.compare x y) c_raw in
    let d_raw = List.sort (fun (x, _) (y, _) -> Pervasives.compare x y) d_raw in
    let a_raw = List.filter (fun (_, v) -> v != 0) a_raw in
    let b_raw = List.filter (fun (_, v) -> v != 0) b_raw in
    let c_raw = List.filter (fun (_, v) -> v != 0) c_raw in
    let d_raw = List.filter (fun (_, v) -> v != 0) d_raw in
    let f1 = Random.int 5 in
    let f2 = Random.int 5 in
    let (x, x_sol) = 
      sprod
        (mul f1 (
            add (a, a_sol) (b, b_sol)))
        (mul f2 (
            add (c, c_sol) (d, d_sol))) in
    let msg = Printf.sprintf "let a = %s, b = %s, c = %s, d = %s, f1 = %d, f2 = %d in (f1*(a + b))*(f2*(c + d)) "
        (show_vec a_raw) (show_vec b_raw) (show_vec c_raw) (show_vec d_raw) f1 f2 in
    Assert.equal ~msg:msg ~prn:string_of_int x_sol x in
  for i = 1 to 10 do
    for j = 0 to 10 do
      for_n i;
    done
  done;
  for i = 0 to 50 do
    for_n ((Random.int 10) + 10)
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
