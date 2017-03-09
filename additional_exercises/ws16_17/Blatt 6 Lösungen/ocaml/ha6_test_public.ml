open Kaputt.Abbreviations
open Ha6_angabe
open Batteries.List

module X = Ha6

let tests = ref []
let points = ref []
let add points_test test =
  points := !points @ [points_test];
  tests := !tests @ [test]

let test_insert () =
  let shuffle d =
    let nd = List.rev_map (fun c -> (Random.bits (), c)) d in
    let sond = List.sort (fun (x, _) (y, _) -> Pervasives.compare x y) nd in
    List.rev_map snd sond in
  let random_list_no_duplicates length =
    let last = ref (-100) in
    let l = init length (fun _ ->
        let next = !last + 1 + (Random.int 5) in
        last := next;
        next) in
    let l_shuff = shuffle l in
    (l, l_shuff) in
  let rec to_sorted_list = function (* !! SOLUTION OF SHEET 5 !! *)
      Node {key; balance; left; right} ->
      (to_sorted_list left) @ [key] @ (to_sorted_list right)
    | Leaf -> [] in
  for i = 10 to 15 do
    let count = i + (i - 10)*1000 in
    let (list_sorted, list_shuffled) = random_list_no_duplicates count in
    let t = fold_left (fun t x -> X.insert x t) Leaf list_shuffled in
    Assert.is_true ~msg:"AVL tree not valid" (X.valid_avl t); 
    let tree_sorted = to_sorted_list t in
    Assert.is_true ~msg:"Elements lost or new elements appeared" (tree_sorted = list_sorted)
  done

let () =
  add 3 @@ Test.make_simple_test ~title:"test_insert" test_insert

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