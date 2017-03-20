let reverse_linewise file_name =
  let input = open_in file_name in
  let rec read acc = try
      let line = input_line input in
      read (line :: acc)
    with End_of_file -> acc in
  let lines_rev = read [] in
  close_in input;
  let lines = List.rev lines_rev in
  let reverse line =
    let splitted = Str.split (Str.regexp " ") line in
    let reversed = List.rev splitted in
    String.concat " " reversed in
  let reversed = List.map reverse lines in
  let output = open_out file_name in
  List.iter (Printf.fprintf output "%s\n") reversed;
  close_out output

let decreasing n =
  let l = ref [] in
  for i = 0 to n do
    l := (i :: !l)
  done;
  !l

exception Negative_argument
exception Zero_argument
exception Odd_argument 

let apply_even_pos f n =
  if n < 0 then
    raise Negative_argument
  else if n = 0 then
    raise Zero_argument
  else if n mod 2 != 0 then
    raise Odd_argument else
    f n

let apply_f_print_error f n =
  try Printf.printf "Result: %d\n" (apply_even_pos f n) with
    Negative_argument -> Printf.printf "Argument is negative!\n"
    | Zero_argument -> Printf.printf "Argument is zero .-(\n"
    | Odd_argument -> Printf.printf "Argument is odd\n"

let () =
  reverse_linewise "/home/jucs/Desktop/blah";
  apply_f_print_error (fun x -> 100 / x) 30;
  apply_f_print_error (fun x -> 2 + x) (-1);
  apply_f_print_error (fun x -> 2 * x) 0;
  apply_f_print_error (fun x -> 2 - x) 13
  