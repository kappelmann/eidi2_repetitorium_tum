open Kaputt.Abbreviations
open Ha7_angabe
open Batteries.List

module X = Ha7
module Sol = Ha7_sol

let tests = ref []
let points = ref []
let add_points points_test test =
  points := !points @ [points_test];
  tests := !tests @ [test]

let test_hyperfib () =
  for i = 0 to 5 do
    let k = (Random.int 10) + 1 in
    let n = (Random.int 55) + 10 in
    Assert.equal ~msg:(Printf.sprintf "hyperfib %d %d" k n) ~prn:string_of_int
      (Sol.hyperfib k n) (X.hyperfib k n)
  done;
  Assert.equal (Sol.hyperfib 20 2) (X.hyperfib 20 2)

let test_coco () =
  for i = 0 to 30 do
    let n = Random.bits () in
    Assert.equal ~msg:(Printf.sprintf "coco %d" n) ~prn:string_of_int
      (Sol.coco n) (X.coco n)
  done;
  Assert.fail_msg "Your tutor needs to check that your function is tail recursive; otherwise it looks fine!"

let test_to_sorted_list () =
  let numbers_sorted = init 2000000 (fun x -> x) in
  let tree = List.fold_left (fun t x -> Node {left = t; right = Leaf; key = x}) Leaf numbers_sorted in
  let tree = Node {left = Node { key = -200; left = Leaf; right = Leaf }; right = tree; key = -100} in
  Assert.equal (X.to_sorted_list tree) ((-200)::(-100)::numbers_sorted);
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
  for i = 0 to 10 do
    let len = Random.int 90 + 10 in
    let (l, l_shuff) = random_list_no_duplicates len in
    let tree = List.fold_left (fun t x -> insert x t) Leaf l_shuff in
    Assert.equal (X.to_sorted_list tree) l
  done

let test_miniml_ctor ctor params_length miniml_prog ocaml_fun =
  for i = 0 to 5 do
    let params = init params_length (fun _ -> Random.int 100) in
    Assert.equal (X.evaluate params miniml_prog) (ctor (ocaml_fun params))
  done

let test_miniml params_length miniml_prog ocaml_fun = test_miniml_ctor
    (fun x -> Integer x) params_length miniml_prog ocaml_fun

let test_miniml_1 () =
  Assert.fail_msg "This exercise needs to be submitted in Moodle and corrected by your tutor!"

let test_miniml_2 () =
  (* _0 - (_1 + 9) *)
  let linear_mini = sub (var "@0") (add (var "@1") (imm 9)) in
  let linear_oc  = function [_0; _1] -> _0 - (_1 + 9)
                          | _ -> assert false in
  test_miniml 2 linear_mini linear_oc;
  (* (!(_0 < 50)) || ((_1 < 50) && (_2 < 50)) *)
  let bool_mini = _or (_not (less _0 (imm 50))) (_and (less _1 (imm 50)) (less _2 (imm 50))) in
  let bool_oc  = function [_0; _1; _2] -> (not (_0 < 50)) || ((_1 < 50) && (_2 < 50))
                        | _ -> assert false in
  test_miniml_ctor (fun x -> Bool x) 3 bool_mini bool_oc

let test_miniml_3 () =
  (* a = _0 + 1; b = 5*_1; a*b - (-5) *)
  let let_mini = _let "a" (add (var "@0") (imm 1))
      (_let "b" (mul (imm 5) (var "@1"))
         (sub (mul (var "a") (var "b")) (neg (imm 5))) ) in
  let let_oc  = function [_0; _1] ->
    let a = _0 + 1 in
    let b = 5 * _1 in
    a*b - (-5)
                       | _ -> assert false in
  test_miniml 2 let_mini let_oc;
  let add_2_mini = _let "add_2" (_fun "x" (add (var "x") (imm 2)))
      (_let "y" (apply (var "add_2") _0)
         (apply (var "add_2") (var "y"))) in
  let add_2_oc = function [_0] -> let add_2 x = x + 2 in
    let y = add_2 _0 in add_2 y
                        | _ -> assert false in
  test_miniml 1 add_2_mini add_2_oc;
  let fun2_mini = _let "add" (_fun "x" (add (var "x") (imm 10)))
      (_let "mul" (_fun "y" (mul (var "y") (imm 2)))
         (sub (apply (var "add") _0) (apply (var "mul") _1))) in
  let fun2_oc = function [_0; _1] -> let add x = x + 10 in
    let mul y = y * 2 in
    (add _0) - (mul _1)
                       | _ -> assert false in
  test_miniml 2 fun2_mini fun2_oc

let test_miniml_4 () =
  let sum_mini = _let_rec "sum"
      (_fun "x" (ite (_eq (var "x") (imm 0))
                   (imm 0)
                   (add (var "x") (apply (var "sum") (sub (var "x") (imm 1))))))
      (apply (var "sum") _0) in
  let sum_oc = function [_0] -> let rec sum x = if x == 0 then 0
                                  else x + (sum (x - 1)) in sum _0
                      | _ -> assert false in
  test_miniml 1 sum_mini sum_oc;
  let fac_mini = _let_rec "fac"
      (_fun "x" (ite (less (var "x") (imm 2))
                   (var "x")
                   (mul (var "x") (apply (var "fac") (sub (var "x") (imm 1))))))
      (apply (var "fac") _0) in
  let fac_oc = function [_0] -> let rec fac x = if x < 2 then x
                                  else x * (fac (x - 1)) in fac _0
                      | _ -> assert false in
  test_miniml 1 fac_mini fac_oc

let test_miniml_5 () =
  let max_plus_5_mini = _let "max"
      (_fun_xx "x" "y"
         (ite (less (var "x") (var "y"))
            (var "y")
            (var "x")))
      (add (apply_xx (var "max") (var "@0") (var "@1")) (imm 5)) in
  let max_plus_5_oc = function [_0; _1] ->
    let max x y = if x < y then y else x in
    (max _0 _1) + 5
                             | _ -> assert false in
  test_miniml 2 max_plus_5_mini max_plus_5_oc;
  let ggt_mini = _let_rec "ggt"
      (_fun_xx "a" "b"
         (ite (_eq (var "a") (imm 0))
            (var "b")
            (ite (_eq (var "b") (imm 0))
               (var "a")
               (ite (less (var "a") (var "b"))
                  (apply_xx (var "ggt") (sub (var "b") (var "a")) (var "a"))
                  (apply_xx (var "ggt") (sub (var "a") (var "b")) (var "b")))
            )
         )
      ) (apply_xx (var "ggt") (var "@0") (var "@1")) in
  let ggt_oc = function [_0; _1] ->
    let rec ggt a b =
      if a == 0 then b
      else if b == 0 then a
      else if a < b then ggt (b - a) a
      else ggt (a - b) a in
    ggt _0 _1
                      | _ -> assert false in
  test_miniml 2 ggt_mini ggt_oc;
  let closure_mini =
    _let "f" (_let "y" _0 (_fun "x" (add (var "x") (var "y"))))
      (apply (var "f") _1) in
  let closure_oc = function [_0; _1] ->
    let f = let y = _0 in fun x -> x + y in
    f _1
                          | _ -> assert false in
  test_miniml 2 closure_mini closure_oc;
  let closure2_mini =
    _let "f" (_let "y" _0
                (_fun "x" (_let "y" _1 (add (var "x") (var "y")))))
      (apply (var "f") _2) in
  let closure2_oc = function [_0; _1; _2] ->
    let f = let _y = _0 in fun x -> let _y = _1 in x + _y in
    f _2
                           | _ -> assert false in
  test_miniml 3 closure2_mini closure2_oc;
  let closure3_mini =
    _let "f" (_let "y" _0
                (_fun "x" (_let "y" _1 (add (var "x") (var "y")))))
      (_let "y" _2 (apply (var "f") _3)) in
  let closure3_oc = function [_0; _1; _2; _3] ->
    let f = let _y = _0 in fun x -> let _y = _1 in x + _y in
    let _y = _2 in f _3
                           | _ -> assert false in
  test_miniml 4 closure3_mini closure3_oc

let () =
  add_points 30 @@ Test.make_simple_test ~title:"test_hyperfib" test_hyperfib;
  add_points 20 @@ Test.make_simple_test ~title:"test_coco" test_coco;
  add_points 50 @@ Test.make_simple_test ~title:"test_to_sorted_list" test_to_sorted_list;
  add_points 10 @@ Test.make_simple_test ~title:"test_miniml_1" test_miniml_1;
  add_points 20 @@ Test.make_simple_test ~title:"test_miniml_2" test_miniml_2;
  add_points 35 @@ Test.make_simple_test ~title:"test_miniml_3" test_miniml_3;
  add_points 35 @@ Test.make_simple_test ~title:"test_miniml_4" test_miniml_4;
  add_points 50 @@ Test.make_simple_test ~title:"test_miniml_5" test_miniml_5

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
