open Kaputt.Abbreviations
open Ha9_angabe
open Batteries.List

module X = Ha9
module Sol = Ha9_sol
let path = "/home/test-runner/scratch"
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

let read_test2 (module Impl : Ha_type) =
  Impl.read_csv { a = ""; b = 0; c = 0.0; d = ""; e = 0 } [
    (fun v curr -> {curr with a = v});
    (fun v curr -> {curr with b = int_of_string v});
    (fun v curr -> {curr with c = float_of_string v});
    (fun v curr -> {curr with d = v});
    (fun v curr -> {curr with e = int_of_string v});
  ] "csv/test2.csv"

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
  Assert.equal csv csv_expected;
  let csv = read_test2 (module X) in
  let csv_expected =
    [{ a = "hallo"; b = 0; c = 0.1; d = "fup"; e = 3 };
     { a = "du"; b = 1; c = 3.1; d = "abc"; e = 99 };
     { a = "flah"; b = 44; c = 9.7; d = "xyz"; e = 99 };
     { a = "flup"; b = 1243; c = 999.; d = "zup"; e = 99 };
     { a = "fump"; b = 0; c = 0.; d = "XhX"; e = 3 }] in
  Assert.equal csv csv_expected

let test_caesav_exn () =
  Assert.make_raises (function
        Failure(x) when x = "int_of_string" -> true
      | _ -> false) Printexc.to_string
    (fun () -> X.read_csv { a = ""; b = 0; c = 0.0; d = ""; e = 0 } [
         (fun v curr -> {curr with a = v});
         (fun v curr -> {curr with b = int_of_string v});
         (fun v curr -> {curr with c = float_of_string v});
         (fun v curr -> {curr with d = v});
         (fun v curr -> {curr with e = int_of_string v});
       ] "csv/test_wrong_data.csv");
  Assert.make_raises (function
        Ha9_angabe.Column_count_does_not_match -> true
      | _ -> false) Printexc.to_string
    (fun () -> X.read_csv { a = ""; b = 0; c = 0.0; d = ""; e = 0 } [
         (fun v curr -> {curr with a = v});
         (fun v curr -> {curr with b = int_of_string v});
         (fun v curr -> {curr with c = float_of_string v});
         (fun v curr -> {curr with d = v});
         (fun v curr -> {curr with e = int_of_string v});
       ] "csv/test_column_missing.csv")

let test_caesav_close () =
  Assert.fail_msg "Your tutor needs to check that you always close open files."

let sort_grouped grouped (!-.-) =
  let sorted_inner = List.map (fun l ->
      List.sort (fun row1 row2 -> String.compare !-.-row1 !-.-row2) l
  ) grouped in
  List.sort (fun l1 l2 ->
      String.compare !-.-(hd l1) !-.-(hd l2)) sorted_inner

let test_group_by () =
  let csv = read_hair (module Sol) in
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
  Assert.equal sorted sorted_expected;
  let csv = read_test2 (module Sol) in
  let grouped = X.group_by csv (fun row -> row.e) in 
  let sorted = sort_grouped grouped (fun x -> x.a) in
  let sorted_expected =
    [[{ a = "du"; b = 1; c = 3.1; d = "abc"; e = 99 };
      { a = "flah"; b = 44; c = 9.7; d = "xyz"; e = 99 };
      { a = "flup"; b = 1243; c = 999.; d = "zup"; e = 99 }];
     [{ a = "fump"; b = 0; c = 0.; d = "XhX"; e = 3 };
      { a = "hallo"; b = 0; c = 0.1; d = "fup"; e = 3 }]] in
  Assert.equal sorted sorted_expected;
  Assert.fail_msg "Your tutor needs to check that your implementation does\
                   not use recursive definitions or imperative features.\
                   Otherwise it looks fine!"

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
  Assert.equal tree_expected tree_actual;
  X.write_xmas tree_path "csv/decor2.csv" 21;
  let tree_actual = read_file tree_path in
  let tree_expected = [
    "                    @                    ";
    "                   ***                   ";
    "                  **do*                  ";
    "                 !**?***                 ";
    "                *O*9*****                ";
    "               ***********               ";
    "              +*********-**              ";
    "             ***************             ";
    "            *****************            ";
    "           abcd*/*******#****>           ";
    "          *********xXx******<**          ";
    "         ****:-)****************         ";
    "        *************************        ";
    "       ***<**o***g****************       ";
    "      ********************X*h*#*1*^      ";
    "     *****<3***.********************     ";
    "    a*i*****************************p    ";
    "   g****#!*****%********$********0****   ";
    "  *************************************  ";
    " **********3**u******=********-*****!*** ";
    "Q***************************************Q";
    "                   ***                   ";
    "                   ***                   ";
    "                   ***                   ";
    "                   ***                   ";
    "                   ***                   ";
    "                   ***                   ";
    "                   ***                   ";
    "                   ***                   ";] in
  Assert.equal tree_expected tree_actual

let test_xmas_unordered () =
  X.write_xmas tree_path "csv/decor1_unordered.csv" 10;
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

let test_xmas_exn () =
  Assert.make_raises (function
        Could_not_decorate -> true
      | _ -> false) Printexc.to_string
    (fun () -> X.write_xmas tree_path "csv/decor1_invalid_level.csv" 10);
  Assert.make_raises (function
        Could_not_decorate -> true
      | _ -> false) Printexc.to_string
    (fun () -> X.write_xmas tree_path "csv/decor1_invalid_position.csv" 10);
      Assert.make_raises (function
        Invalid_decoration("ab") -> true
      | _ -> false) Printexc.to_string
    (fun () -> X.write_xmas tree_path "csv/decor1_invalid_decor.csv" 10)

let test_xmas_close () =
  Assert.fail_msg "Your tutor needs to check that you always close open files."

let () =
  add 50 @@ Test.make_simple_test ~title:"test_caesav" test_caesav;
  add 20 @@ Test.make_simple_test ~title:"test_caesav_exn" test_caesav_exn;
  add 10 @@ Test.make_simple_test ~title:"test_caesav_close" test_caesav_close;
  add 40 @@ Test.make_simple_test ~title:"test_group_by" test_group_by;
  add 40 @@ Test.make_simple_test ~title:"test_xmas" test_xmas;
  add 10 @@ Test.make_simple_test ~title:"test_xmas_unordered" test_xmas_unordered;
  add 20 @@ Test.make_simple_test ~title:"test_xmas_exn" test_xmas_exn;
  add 10 @@ Test.make_simple_test ~title:"test_xmas_close" test_xmas_close

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
