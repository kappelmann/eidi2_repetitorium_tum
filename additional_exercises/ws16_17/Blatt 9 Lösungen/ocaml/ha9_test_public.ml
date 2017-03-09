open Kaputt.Abbreviations
open Ha9_angabe
open Batteries.List

module X = Ha9
let path = "."
module type Ha_type = module type of X

let tests = ref []
let points = ref []
let add points_test test =
  points := !points @ [points_test];
  tests := !tests @ [test]

type xmas_hair_row = { name : string; expected_presents : int; hair_length : float }
[@@deriving show]

type test2_row = { a : string; b : int; c : float; d : string; e : int }
[@@deriving show]

let read_hair (module Impl : Ha_type) =
  Impl.read_csv {name = ""; expected_presents = 0; hair_length = 0.0} [
    (fun v curr -> {curr with name = v});
    (fun v curr -> {curr with expected_presents = int_of_string v});
    (fun v curr -> {curr with hair_length = float_of_string v})] "csv/xmas_hair.csv"

let test_caesav () =
  let csv = read_hair (module X) in
  let csv_expected =
    [{ name = "Hugo"; expected_presents = 99; hair_length = 2.1 };
     { name = "Inge"; expected_presents = 11; hair_length = 99.1 };
     { name = "Egon"; expected_presents = 11; hair_length = 0.2 };
     { name = "Frida"; expected_presents = 1000; hair_length = 100.2 };
     { name = "Paula"; expected_presents = 0; hair_length = 21. };
     { name = "Ero"; expected_presents = 0; hair_length = 0. };
     { name = "Fipsi"; expected_presents = 199; hair_length = 22. }] in
  Assert.equal csv csv_expected

let sort_grouped grouped (!-.-) =
  let sorted_inner = List.map (fun l ->
      List.sort (fun row1 row2 -> String.compare !-.-row1 !-.-row2) l
  ) grouped in
  List.sort (fun l1 l2 ->
      String.compare !-.-(hd l1) !-.-(hd l2)) sorted_inner

let test_group_by () =
  let csv = read_hair (module X) in
  let grouped = X.group_by csv (fun row -> row.expected_presents) in 
  let sorted = sort_grouped grouped (fun x -> x.name) in
  let sorted_expected =
    [[{ name = "Egon"; expected_presents = 11; hair_length = 0.2 };
      { name = "Inge"; expected_presents = 11; hair_length = 99.1 }];
     [{ name = "Ero"; expected_presents = 0; hair_length = 0. };
      { name = "Paula"; expected_presents = 0; hair_length = 21. }];
     [{ name = "Fipsi"; expected_presents = 199; hair_length = 22. }];
     [{ name = "Frida"; expected_presents = 1000; hair_length = 100.2 } ];
     [{ name = "Hugo"; expected_presents = 99; hair_length = 2.1 }]] in
  Assert.equal sorted sorted_expected

let protect ~f ~finally = try
    let result = f () in finally (); result with
  ex -> finally (); raise ex

let read_file file_name =
  let file = open_in file_name in
  protect ~f:(fun () ->
      let rec read acc =
        try let line = input_line file in
          read (line::acc) with
          End_of_file -> acc in
      List.rev (read [])
    )
    ~finally:(fun () -> close_in file)

let tree_path = path ^ "/tree"

let test_xmas () =
  X.write_xmas tree_path "csv/decor1.csv" 10;
  let tree_actual = read_file tree_path in
  let tree_expected = [
    "         @         ";
    "        a**        ";
    "       *****       ";
    "      **x**~*      ";
    "     **4*****1     ";
    "    **G**0*2***    ";
    "   *******O*X***   ";
    "  **+***~********  ";
    " ****!!#***<>***<> ";
    "o*o*o*o*o*o*o*o*o*o";
    "        ***        ";
    "        ***        ";
    "        ***        ";
    "        ***        "] in
  Assert.equal tree_expected tree_actual

let () =
  add 0 @@ Test.make_simple_test ~title:"test_caesav" test_caesav;
  add 0 @@ Test.make_simple_test ~title:"test_group_by" test_group_by;
  add 0 @@ Test.make_simple_test ~title:"test_xmas" test_xmas

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
