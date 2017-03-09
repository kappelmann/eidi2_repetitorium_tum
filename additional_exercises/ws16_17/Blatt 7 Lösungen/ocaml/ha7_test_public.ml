open Kaputt.Abbreviations
open Ha7_angabe
open Batteries.List

module X = Ha7

let tests = ref []
let points = ref []
let add_points points_test test =
  points := !points @ [points_test];
  tests := !tests @ [test]

let test_to_sorted_list () =
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
  (* _0 + (_1 * 2) *)
  let linear_mini = add (var "@0") (mul (var "@1") (imm 2)) in
  let linear_oc  = function [_0; _1] -> _0 + (_1 * 2)
                          | _ -> assert false in
  test_miniml 2 linear_mini linear_oc;
  let bool_mini = _or (_not (less _0 (imm 30))) (_and (less _1 (imm 30)) (less _2 (imm 30))) in
  let bool_oc  = function [_0; _1; _2] -> (not (_0 < 30)) || ((_1 < 30) && (_2 < 30))
                        | _ -> assert false in
  test_miniml_ctor (fun x -> Bool x) 3 bool_mini bool_oc

let test_miniml_3 () =
  let add_2_mini = _let "add_2" (_fun "x" (add (var "x") (imm 2)))
      (_let "y" (apply (var "add_2") _0)
         (apply (var "add_2") (var "y"))) in
  let add_2_oc = function [_0] -> let add_2 x = x + 2 in
    let y = add_2 _0 in add_2 y
                        | _ -> assert false in
  test_miniml 1 add_2_mini add_2_oc

let test_miniml_4 () =
  let sum_mini = _let_rec "sum"
      (_fun "x" (ite (_eq (var "x") (imm 0))
                   (imm 0)
                   (add (var "x") (apply (var "sum") (sub (var "x") (imm 1))))))
      (apply (var "sum") _0) in
  let sum_oc = function [_0] -> let rec sum x = if x == 0 then 0
                                  else x + (sum (x - 1)) in sum _0
                      | _ -> assert false in
  test_miniml 1 sum_mini sum_oc

let test_miniml_5 () =
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
  test_miniml 2 closure_mini closure_oc

let () =
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
