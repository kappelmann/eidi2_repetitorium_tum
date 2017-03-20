open Kaputt.Abbreviations
open Batteries.LazyList

module X = Ha5

let tests = ref []
let points = ref []
let add points_test test =
  points := !points @ [points_test];
  tests := !tests @ [test]

let test_inv_cantor () =
  for i = 0 to 30 do
    let x = Random.int 500 in
    let y = Random.int 500 in
    let cantor = y + (x + y)*(x + y + 1)/2 in
    let (x', y') = X.inv_cantor cantor in
    Assert.equal (x', y') (x, y)
  done

let random_list len =
  let lazy_numbers = from_while (fun () -> Some (Random.int 1000)) in
  to_list (take len lazy_numbers)

let random_sorted len =
  let lazy_numbers = unfold 0 (fun x ->
      let next = x + (Random.int 5) in Some (next, next)) in
  to_list (take len lazy_numbers)

let is_sorted f l =
  let (sorted, _) = List.fold_left (fun (sorted, last) next -> (sorted && (match last with
        Some last -> (f last next)
      | None -> true), Some next)) (true, None) l
  in sorted

let test_is_insert () =
  for i = 0 to 30 do
    let len = Random.int 100 in
    let numbers = random_list len in
    List.iter (fun f ->
        let numbers_sorted = List.sort (fun x y ->
            if f x y then -1 else
            if f y x then 1 else 0) numbers in
        let x = Random.int 100 in
        Assert.is_true (is_sorted f (X.is_insert f x numbers_sorted)))
      [(<=); (>=)]
  done

let test_insertion_sort () =
  for i = 0 to 30 do
    let len = Random.int 100 in
    let numbers = random_list len in
    List.iter (fun f ->
        let numbers_sorted = X.insertion_sort numbers f in
        Assert.is_true (is_sorted f numbers_sorted))
      [(<=); (>=)]
  done

let test_tree sorter =
  let persons = [{ X.name = "Ilse"; X.age = 7 };
                 { X.name = "Iber"; X.age = 3 };
                 { X.name = "Julian"; X.age = 199 };
                 { X.name = "Julia"; X.age = 9 };
                 { X.name = "Foo"; X.age = 12 };
                 { X.name = "Bar"; X.age = 11 };
                 { X.name = "Trabor"; X.age = 11 }] in
  let tree = List.fold_left (fun tree p -> X.insert p tree)
      (X.singleton { X.name = "Lulu"; X.age = 21 }) persons in
  let tree_sorted = sorter tree in
  let name_list = List.map (fun x -> x.X.name) tree_sorted in
  Assert.is_true (is_sorted (fun x y -> String.compare x y < 0) name_list)

let test_to_sorted_list () =
  test_tree X.to_sorted_list

let () =
  add 20 @@ Test.make_simple_test ~title:"test_inv_cantor" test_inv_cantor;
  add 10 @@ Test.make_simple_test ~title:"test_is_insert" test_is_insert;
  add 10 @@ Test.make_simple_test ~title:"test_insertion_sort" test_insertion_sort;
  add 15 @@ Test.make_simple_test ~title:"test_to_sorted_list" test_to_sorted_list

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
